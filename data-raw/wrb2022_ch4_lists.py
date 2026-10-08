# Extract the per-RSG qualifier lists of WRB 2022, Chapter 4 ("Key to the
# Reference Soil Groups with lists of principal and supplementary qualifiers"),
# from the official PDF, to audit inst/rules/wrb2022/qualifiers.yaml.
#
#   curl -o /tmp/wrb2022.pdf https://files.isric.org/public/documents/WRB_fourth_edition_2022-12-18.pdf
#   pdftotext -layout /tmp/wrb2022.pdf /tmp/wrb2022.txt
#   python3 data-raw/wrb2022_ch4_lists.py /tmp/wrb2022.txt /tmp/wrb2022_ch4.json
#
# Each RSG has its own page: the key text on the left, the principal
# qualifiers in a middle column and the supplementary ones on the right.
# Alternatives keep WRB's slash ("Rhodic/Xanthic"); an alternative wrapped
# over two lines ("Arenic/ Clayic/ Loamic/" + "Siltic") is joined. Footnote
# digits after an RSG name ("TECHNOSOLS1") are dropped. The PDF is not
# redistributed here: the IUSS / FAO text is read from ISRIC at use.
#
# Used in v0.9.216 to find the ten qualifiers that do not exist in WRB 2022,
# and to measure how far the lists are from Chapter 4.
import re, json, sys
txt = open(sys.argv[1], encoding="utf-8").read()
pages = txt.split("\f")
RSG = ["HISTOSOLS","ANTHROSOLS","TECHNOSOLS","CRYOSOLS","LEPTOSOLS","SOLONETZ","VERTISOLS","SOLONCHAKS","GLEYSOLS","ANDOSOLS","PODZOLS","PLINTHOSOLS","PLANOSOLS","STAGNOSOLS","NITISOLS","FERRALSOLS","CHERNOZEMS","KASTANOZEMS","PHAEOZEMS","UMBRISOLS","DURISOLS","GYPSISOLS","CALCISOLS","RETISOLS","ACRISOLS","LIXISOLS","ALISOLS","LUVISOLS","CAMBISOLS","FLUVISOLS","ARENOSOLS","REGOSOLS"]
out = {}
for pg in pages:
    lines = pg.split("\n")
    hdr = [i for i, l in enumerate(lines) if "Key to the Reference Soil Groups" in l and "Principal qualifiers" in l]
    if not hdr: continue
    h = hdr[0]
    pcol = lines[h].index("Principal qualifiers") + len("Principal qualifiers") / 2
    scol = None
    for l in lines[max(0, h - 2):h + 2]:
        m = l.find("Supplementary")
        if m >= 0: scol = m + len("Supplementary") / 2
    name = None; prin = []; supp = []; pend = {"p": "", "s": ""}
    for l in lines[h + 2:]:
        if "Overview of Key" in l: break
        for m in re.finditer(r"\S+(?: \S+)*", l):
            tok, start = m.group(0), m.start()
            bare = re.sub(r"\d+$", "", tok)
            if start < 8 and bare in RSG: name = bare; continue
            centre = start + len(tok) / 2
            if start < 30: continue          # key text
            dp, ds = abs(centre - pcol), abs(centre - scol)
            if min(dp, ds) > 22: continue
            if bare in RSG: name = bare; continue
            q = re.sub(r"\d+$", "", tok).replace("/ ", "/")
            # qualifier tokens look like Capitalised words, optional "/Word", maybe a trailing "/"
            if not re.fullmatch(r"[A-Z][a-z]+(?:/[A-Z][a-z]+)*/?", q): continue
            col = "p" if dp < ds else "s"
            q = pend[col] + q; pend[col] = ""
            if q.endswith("/"): pend[col] = q; continue
            (prin if col == "p" else supp).append(q)
    if name: out[name] = {"principal": prin, "supplementary": supp}
json.dump(out, open(sys.argv[2], "w"), indent=1)
print(len(out), "RSGs parsed")
