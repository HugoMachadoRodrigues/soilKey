/* soilKey Pro: file uploads and downloads over the Shiny websocket, and a
   notice when the connection to the server is lost (v0.9.303).

   Shiny's own file input and download links use plain HTTP requests to the
   R process that holds the session. With more than one instance behind the
   load balancer such a request can reach another process and fail with 404,
   so the files travel over the websocket instead. The server side is
   R/utils_transfer.R. */
(function () {
  'use strict';

  var SLICE = 512 * 1024;   // bytes per upload slice
  var waiting = {};         // upload id -> function(error) run on the ack
  var incoming = {};        // download id -> the decoded slices so far

  function newId() {
    return Date.now().toString(36) +
      Math.floor(Math.random() * 1e9).toString(36);
  }

  function toBase64(buf) {
    var bytes = new Uint8Array(buf), out = '', step = 0x8000;
    for (var i = 0; i < bytes.length; i += step) {
      out += String.fromCharCode.apply(null, bytes.subarray(i, i + step));
    }
    return window.btoa(out);
  }

  function fromBase64(s) {
    var bin = window.atob(s || ''), n = bin.length, bytes = new Uint8Array(n);
    for (var i = 0; i < n; i++) bytes[i] = bin.charCodeAt(i);
    return bytes;
  }

  function readSlice(file, a, b) {
    var blob = file.slice(a, b);
    if (blob.arrayBuffer) return blob.arrayBuffer();
    return new Promise(function (resolve, reject) {
      var r = new FileReader();
      r.onload = function () { resolve(r.result); };
      r.onerror = function () { reject(r.error); };
      r.readAsArrayBuffer(blob);
    });
  }

  // ---- upload --------------------------------------------------------------

  function widgets(input) {
    var box = input.closest('.shiny-input-container');
    return {
      text: box ? box.querySelector('input[type=text]') : null,
      progress: box ? box.querySelector('.progress') : null,
      bar: box ? box.querySelector('.progress-bar') : null
    };
  }

  function setBar(w, frac, label, failed) {
    if (!w.progress || !w.bar) return;
    w.progress.style.visibility = 'visible';
    w.progress.classList.toggle('active', !label);
    w.bar.classList.toggle('progress-bar-danger', !!failed);
    w.bar.classList.toggle('bg-danger', !!failed);
    w.bar.style.width = Math.round(100 * frac) + '%';
    w.bar.textContent = label || '';
  }

  function accepted(input, file) {
    var acc = (input.getAttribute('accept') || '').trim();
    if (!acc) return true;
    var name = (file.name || '').toLowerCase(), type = (file.type || '').toLowerCase();
    return acc.split(',').some(function (tok) {
      tok = tok.trim().toLowerCase();
      if (!tok) return false;
      if (tok.charAt(0) === '.') return name.slice(-tok.length) === tok;
      if (tok.slice(-2) === '/*') return type.indexOf(tok.slice(0, -1)) === 0;
      return type === tok;
    });
  }

  function upload(input, file) {
    var id = input.getAttribute('data-sk-file');
    var max = Number(input.getAttribute('data-sk-max') || -1);
    var failMsg = input.getAttribute('data-sk-failed') || 'Upload failed';
    var w = widgets(input);
    if (w.text) w.text.value = file.name;
    if (max >= 0 && file.size > max) {
      setBar(w, 1, input.getAttribute('data-sk-too-large') || failMsg, true);
      return;
    }
    if (!window.Shiny || !Shiny.shinyapp || !Shiny.shinyapp.isConnected()) {
      setBar(w, 1, failMsg, true);
      return;
    }
    var uid = newId(), n = Math.max(1, Math.ceil(file.size / SLICE)), k = 0;
    setBar(w, 0, '', false);

    function next() {
      if (k >= n) {
        Shiny.setInputValue(id + ':sk.file',
          {uid: uid, name: file.name, size: file.size, type: file.type || ''},
          {priority: 'event'});
        setBar(w, 1, input.getAttribute('data-sk-done') || 'Upload complete', false);
        return;
      }
      readSlice(file, k * SLICE, Math.min(file.size, (k + 1) * SLICE)).then(function (buf) {
        waiting[uid] = function (error) {
          delete waiting[uid];
          if (error) {
            setBar(w, 1, error === 'too_large' ?
              (input.getAttribute('data-sk-too-large') || failMsg) : failMsg, true);
            return;
          }
          k += 1;
          setBar(w, k / n, '', false);
          next();
        };
        var msg = {uid: uid, k: k, n: n, data: toBase64(buf)};
        if (k === 0) msg.name = file.name;
        Shiny.setInputValue(id + '__sk_chunk:sk.file.chunk', msg, {priority: 'event'});
      }, function () {
        setBar(w, 1, failMsg, true);
      });
    }
    next();
  }

  function onAck(m) {
    var cb = waiting[m.uid];
    if (cb) cb(m.error || null);
  }

  document.addEventListener('change', function (e) {
    var el = e.target;
    if (!el || !el.matches || !el.matches('input[type=file][data-sk-file]')) return;
    var f = el.files && el.files[0];
    if (f) upload(el, f);
    el.value = '';   // so the same file can be chosen again
  });

  // drag and drop onto the file box, as Shiny's own file input allows
  function dropTarget(t) {
    var box = t && t.closest ? t.closest('.shiny-input-container') : null;
    return box ? box.querySelector('input[type=file][data-sk-file]') : null;
  }
  function overClass(input, on) {
    var w = widgets(input);
    if (w.text) w.text.classList.toggle('shiny-file-input-over', on);
  }
  document.addEventListener('dragover', function (e) {
    var input = dropTarget(e.target);
    if (!input) return;
    e.preventDefault();
    overClass(input, true);
  });
  document.addEventListener('dragleave', function (e) {
    var input = dropTarget(e.target);
    if (input) overClass(input, false);
  });
  document.addEventListener('drop', function (e) {
    var input = dropTarget(e.target);
    if (!input) return;
    e.preventDefault();
    overClass(input, false);
    var f = e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files[0];
    if (f && accepted(input, f)) upload(input, f);
  });

  // ---- download ------------------------------------------------------------

  function save(blob, name) {
    var url = URL.createObjectURL(blob), a = document.createElement('a');
    a.href = url;
    a.download = name || 'download';
    a.rel = 'noopener';
    a.style.display = 'none';
    document.body.appendChild(a);
    a.click();
    setTimeout(function () { URL.revokeObjectURL(url); a.remove(); }, 60000);
  }

  function onDownload(m) {
    var parts = incoming[m.uid] || (incoming[m.uid] = []);
    parts[m.k] = fromBase64(m.data);
    if (m.k < m.n - 1) return;          // messages arrive in order
    delete incoming[m.uid];
    save(new Blob(parts, {type: m.type || 'application/octet-stream'}), m.name);
  }

  // ---- connection lost -----------------------------------------------------

  function lostNotice(show) {
    var b = document.getElementById('sk_disconnected');
    if (!b) return;
    b.hidden = !show;
    if (show) {
      var btn = document.getElementById('sk_disconnected_reload');
      if (btn) btn.focus();
    }
  }

  document.addEventListener('click', function (e) {
    if (e.target.closest && e.target.closest('#sk_disconnected_reload')) {
      window.location.reload();
    }
  });

  function setup() {
    Shiny.addCustomMessageHandler('sk_file_ack', onAck);
    Shiny.addCustomMessageHandler('sk_download', onDownload);
    if (window.jQuery) {
      // Shiny triggers these through jQuery
      window.jQuery(document).on('shiny:disconnected', function () { lostNotice(true); });
      window.jQuery(document).on('shiny:connected', function () { lostNotice(false); });
    }
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) setup();
  else document.addEventListener('DOMContentLoaded', setup);
})();
