# Writes inst/rules/za1991/families.yaml: the 404 families of the 73 soil
# forms of the South African soil classification of 1991 (Soil
# Classification Working Group, 1991), their codes (the form code and the
# four digits printed by the AGIS key) and names, and the 19 family
# criteria. Only codes, names, page numbers and soilKey's class keys are
# written; no text of the sources. The tables below were read once from the
# family table of the AGIS key pages (the classes of each digit, in the
# order the key prints them), with the printing slips read as in plan
# section 10: Plooysburg 1000 and 2000 (printed "100"), the class headers
# "1110" (Valsrivier, Swartland, Etosha, Addo) and "2110>" (Swartland) read
# 1100 and 2110 (headers only, no family code changes), "131" and "Bi" read
# B1, the Shortlands 3000 families read on the B1 like the other Shortlands
# families, and the signs of wetness "in carbonate horizon" read on the soft
# carbonate horizon (FC 15). On the review (Hugo, 2026-11-06): the first two
# Shortlands classes need the whole B non-calcareous (R3), and the signs of
# wetness of Dundee are read from the stratified alluvium down (R2).
#
# A condition is written criterion:part:horizon=class (several classes
# joined by "|": "mesotrophic or eutrophic"); conditions joined by " & "
# all hold. The part of the horizon read (R/families-za.R):
#   whole       all the layers of the horizon
#   B1          the upper layer of the horizon, whatever its designation
#               (Q20; R5, Hugo, 2026-11-06)
#   upper       its first layer;  lower  its last layer
#   below       the first layer under it;  below_all  every layer under it
#   with_below  the horizon and the first layer under it
#   from        the horizon and every layer under it
#   with_next   the horizon and the first layer of the horizon under it
#   profile     every layer within 1500 mm (horizon "-")
# The horizon is the token of horizons.yaml (the first horizon of the form's
# sequence with that token).
#
# Run from the package root: Rscript data-raw/za1991_families.R

digits <- "
Ch     1 1:whole:OO=fibrous ; 1:whole:OO=humified
Ch     2 19:below:OO=rock_or_saprolite ; 19:below:OO=unconsolidated
Kp     1 2:whole:HA=thin ; 2:whole:HA=thick
Kp     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Ma     1 2:whole:HA=thin ; 2:whole:HA=thick
Ma     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
[Ia]   1 2:whole:HA=thin ; 2:whole:HA=thick
[Ia]   2 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Lu     1 2:whole:HA=thin ; 2:whole:HA=thick
Lu     2 9:whole:PCU=non_red ; 9:whole:PCU=red
Lu     3 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Sr     1 2:whole:HA=thin ; 2:whole:HA=thick
Sr     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Sr     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
No     1 2:whole:HA=thin ; 2:whole:HA=thick
No     2 14:B1:LCU=not_hard ; 14:B1:LCU=hard
Rg     1 16:upper:G=non_calcareous ; 16:upper:G=calcareous
Ar     1 5:whole:VA=dark ; 5:whole:VA=red ; 5:whole:VA=other
Ar     2 16:whole:VA=non_calcareous ; 16:whole:VA=calcareous
Wo     1 16:upper:G=non_calcareous ; 16:upper:G=calcareous
Bo     1 5:whole:PCU=dark ; 5:whole:PCU=red ; 5:whole:PCU=other
Bo     2 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Bo     3 16:whole:PCU=non_calcareous ; 16:whole:PCU=calcareous
Sn     1 16:whole:MA=non_calcareous ; 16:whole:MA=calcareous
Im     1 16:whole:MA=non_calcareous ; 16:whole:MA=calcareous
My     1 14:B1:LCU=not_hard ; 14:B1:LCU=hard
My     2 16:whole:LCU=non_calcareous ; 16:whole:LCU=calcareous
Mw     1 16:whole:MA=non_calcareous ; 16:whole:MA=calcareous
Ik     1 15:with_below:MA=absent ; 15:with_below:MA=present
Ik     2 16:with_below:MA=non_calcareous ; 16:with_below:MA=calcareous
Ka     1 16:whole:G=non_calcareous ; 16:whole:G=calcareous
Kd     1 6:whole:E=grey ; 6:whole:E=yellow
Lo     1 6:whole:E=grey ; 6:whole:E=yellow
Wa     1 6:whole:E=grey ; 6:whole:E=yellow
Ct     1 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Ct     2 17:below_all:YAB=absent ; 17:below_all:YAB=present
Ts     1 15:below:PLP=absent ; 15:below:PLP=present
Lt     1 13:whole:PZB=not_hardened ; 13:whole:PZB=hardened
Lt     2 18:upper:UMW=friable ; 18:upper:UMW=firm
Cc     1 18:upper:UMN=friable ; 18:upper:UMN=firm
Hh     1 14:upper:SAP=not_hard ; 14:upper:SAP=hard
Hh     2 15:below:PZB=absent ; 15:below:PZB=present
Es     1 6:whole:E=grey ; 6:whole:E=yellow
Es     2 12:whole:PRC=without ; 12:whole:PRC=with
Km     1 6:whole:E=grey ; 6:whole:E=yellow
Km     2 9:whole:PCU=non_red ; 9:whole:PCU=red
Km     3 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Vf     1 6:whole:E=grey ; 6:whole:E=yellow
Vf     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Vf     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Kk     1 6:whole:E=grey ; 6:whole:E=yellow
Kk     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Kk     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Cf     1 6:whole:E=grey ; 6:whole:E=yellow
Cf     2 14:B1:LCU=not_hard ; 14:B1:LCU=hard
Fw     1 3:whole:OA=light ; 3:whole:OA=dark
Fw     2 6:whole:E=grey ; 6:whole:E=yellow
Fw     3 7:whole:E=absent ; 7:whole:E=present
We     1 10:B1:SPL=non_luvic ; 10:B1:SPL=luvic
Dr     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Av     1 8:B1:YAB=dystrophic ; 8:B1:YAB=mesotrophic ; 8:B1:YAB=eutrophic
Av     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Gc     1 8:B1:YAB=dystrophic ; 8:B1:YAB=mesotrophic ; 8:B1:YAB=eutrophic
Gc     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Pn     1 8:B1:YAB=dystrophic ; 8:B1:YAB=mesotrophic ; 8:B1:YAB=eutrophic
Pn     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Gf     1 8:B1:YAB=dystrophic ; 8:B1:YAB=mesotrophic|eutrophic
Gf     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Mp     1 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Mp     2 15:whole:SCH=absent ; 15:whole:SCH=present
Ak     1 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Cv     1 8:B1:YAB=dystrophic ; 8:B1:YAB=mesotrophic ; 8:B1:YAB=eutrophic
Cv     2 10:B1:YAB=non_luvic ; 10:B1:YAB=luvic
Bv     1 8:B1:RAB=dystrophic ; 8:B1:RAB=mesotrophic ; 8:B1:RAB=eutrophic
Bv     2 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Bd     1 8:B1:RAB=dystrophic ; 8:B1:RAB=mesotrophic ; 8:B1:RAB=eutrophic
Bd     2 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Ky     1 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Ky     2 15:whole:SCH=absent ; 15:whole:SCH=present
[Py]   1 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Gr     1 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Hu     1 8:B1:RAB=dystrophic ; 8:B1:RAB=mesotrophic ; 8:B1:RAB=eutrophic
Hu     2 10:B1:RAB=non_luvic ; 10:B1:RAB=luvic
Sd     1 8:B1:RSB=dystrophic|mesotrophic & 16:whole:RSB=non_calcareous ; 8:B1:RSB=eutrophic & 16:whole:RSB=non_calcareous ; 16:whole:RSB=calcareous
Sd     2 10:B1:RSB=non_luvic ; 10:B1:RSB=luvic
Sd     3 11:whole:RSB=subangular_fine ; 11:whole:RSB=medium_coarse
Jb     1 15:below:PLP=absent ; 15:below:PLP=present
Wf     1 13:whole:PZB=not_hardened ; 13:whole:PZB=hardened
Wf     2 18:upper:UMW=friable ; 18:upper:UMW=firm
Pg     1 18:upper:UMN=friable ; 18:upper:UMN=firm
Gk     1 14:upper:SAP=not_hard ; 14:upper:SAP=hard
Gk     2 15:below:PZB=absent ; 15:below:PZB=present
Ss     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Ss     2 9:whole:PRC=non_red ; 9:whole:PRC=red
Se     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Se     2 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Se     3 16:with_next:PCU=non_calcareous ; 16:with_next:PCU=calcareous
Va     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Va     2 9:whole:PCU=non_red ; 9:whole:PCU=red
Va     3 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Va     4 16:with_next:PCU=non_calcareous ; 16:with_next:PCU=calcareous
Sw     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Sw     2 9:whole:PCU=non_red ; 9:whole:PCU=red
Sw     3 11:whole:PCU=subangular_fine ; 11:whole:PCU=medium_coarse
Sw     4 16:with_next:PCU=non_calcareous ; 16:with_next:PCU=calcareous
Tu     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Tu     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Tu     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Et     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Et     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Et     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Et     4 15:whole:SCH=absent ; 15:whole:SCH=present
Gm     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Gm     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Gm     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Ou     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Ou     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Ou     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Oa     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Oa     2 9:whole:NCU=non_red ; 9:whole:NCU=red
Oa     3 10:B1:NCU=non_luvic ; 10:B1:NCU=luvic
Mu     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Mu     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Mu     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Ad     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Ad     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Ad     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Ad     4 15:whole:SCH=absent ; 15:whole:SCH=present
Pr     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Pr     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Pr     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Tr     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Tr     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Tr     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Ag     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Ag     2 9:whole:NCA=non_red ; 9:whole:NCA=red
Ag     3 10:B1:NCA=non_luvic ; 10:B1:NCA=luvic
Br     1 15:whole:SCH=absent ; 15:whole:SCH=present
Cg     1 16:whole:OA=non_calcareous ; 16:whole:OA=calcareous
Kn     1 16:whole:OA=non_calcareous ; 16:whole:OA=calcareous
Gs     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Gs     2 14:B1:LCU=not_hard ; 14:B1:LCU=hard
Gs     3 15:B1:LCU=absent ; 15:B1:LCU=present
Gs     4 16:whole:LCU=non_calcareous ; 16:whole:LCU=calcareous
Ms     1 4:whole:OA=not_bleached ; 4:whole:OA=bleached
Ms     2 16:whole:OA=non_calcareous ; 16:whole:OA=calcareous
Du     1 9:whole:SAL=non_red ; 9:whole:SAL=red
Du     2 15:from:SAL=absent ; 15:from:SAL=present
Du     3 16:profile:-=non_calcareous ; 16:profile:-=calcareous
Nb     1 9:whole:RSA=non_red ; 9:whole:RSA=red
Nb     2 16:profile:-=non_calcareous ; 16:profile:-=calcareous
Wb     1 16:profile:-=non_calcareous ; 16:profile:-=calcareous
"

families <- "
Ch     1100 Gwaing
Ch     1200 Manhica
Ch     2100 Graskop
Ch     2200 Rietfontein
Kp     1100 Fordoun
Kp     1200 Dargle
Kp     2100 Stonyhill
Kp     2200 Nungwane
Ma     1100 Glenesk
Ma     1200 Connemara
Ma     2100 Lambasi
Ma     2200 Ntsubane
[Ia]   1100 Himeville
[Ia]   1200 Highlands
[Ia]   2100 Mayfield
[Ia]   2200 Glenariff
Lu     1110 Erradale
Lu     1120 Argyll
Lu     1210 Woodland
Lu     1220 Hopewell
Lu     2110 Matata
Lu     2120 Coleraine
Lu     2210 Dubana
Lu     2220 Clifton
Sr     1110 Uplands
Sr     1120 Glenwood
Sr     1210 Lincoln
Sr     1220 Winshaw
Sr     2110 Newton
Sr     2120 Copling
Sr     2210 Aberdare
Sr     2220 Fielden
No     1100 Boston
No     1200 Overwood
No     2100 Deepdene
No     2200 Peakvale
Rg     1000 Greendale
Rg     2000 Rietkuil
Ar     1100 Lonehill
Ar     1200 Rustenburg
Ar     2100 Minerva
Ar     2200 Diepsloot
Ar     3100 Bospoort
Ar     3200 Deercroft
Wo     1000 Ottawa
Wo     2000 Kromdal
Bo     1110 Eureka
Bo     1120 Salimba
Bo     1210 Windermere
Bo     1220 Onrus
Bo     2110 Golela
Bo     2120 Rustig
Bo     2210 Rockvale
Bo     2220 Bulhoek
Bo     3110 Tembani
Bo     3120 Vlakplaas
Bo     3210 Lutengele
Bo     3220 Mkuze
Sn     1000 Duivenhoks
Sn     2000 Brabant
Im     1000 Kilkenny
Im     2000 Kalkpan
My     1100 Glenecho
My     1200 Legdaar
My     2100 Grassmere
My     2200 Boyela
Mw     1000 Effingham
Mw     2000 Mpetu
Ik     1100 Oatlands
Ik     1200 Shingwedzi
Ik     2100 Headford
Ik     2200 Sterkfontein
Ka     1000 Lammermoor
Ka     2000 Slangspruit
Kd     1000 Morgendal
Kd     2000 Grabouw
Lo     1000 Sherbrook
Lo     2000 Ermelo
Wa     1000 Louterwater
Wa     2000 Lynedoch
Ct     1100 Potberg
Ct     1200 Philippi
Ct     2100 Papegaaikop
Ct     2200 Thesen
Ts     1000 Bakenkop
Ts     2000 Kurland
Lt     1100 Kruisfontein
Lt     1200 Langbos
Lt     2100 Laprovence
Lt     2200 Drakenstein
Cc     1000 Spioenkop
Cc     2000 Windheuwel
Hh     1100 Bergplaas
Hh     1200 Keurrug
Hh     2100 Kouma
Hh     2200 Ysternek
Es     1100 Zastron
Es     1200 Nuweplaas
Es     2100 Chiltern
Es     2200 Haarlem
Km     1110 Broadline
Km     1120 Napier
Km     1210 Ramsgate
Km     1220 Mangeti
Km     2110 Wellington
Km     2120 Bossieveld
Km     2210 Longdown
Km     2220 Humansdorp
Vf     1110 Alexandria
Vf     1120 Woburn
Vf     1210 Renishaw
Vf     1220 Freeland
Vf     2110 Blikhuis
Vf     2120 Buffelsbaai
Vf     2210 Glentana
Vf     2220 Jongensfontein
Kk     1110 Nyanga
Kk     1120 Baievlei
Kk     1210 Alfred
Kk     1220 Oakhill
Kk     2110 Bluedowns
Kk     2120 Laaiplek
Kk     2210 Voltas
Kk     2220 Voorbaai
Cf     1100 Frosterley
Cf     1200 Egolomi
Cf     2100 Steenbras
Cf     2200 Witzenberg
Fw     1110 Penicuik
Fw     1120 Cornubia
Fw     1210 Hopefield
Fw     1220 Duinzicht
Fw     2110 Waterton
Fw     2120 Kwambonambi
Fw     2210 Rosa
Fw     2220 Keimouth
We     1000 Helena
We     2000 Mareetsane
Dr     1000 Tevreden
Dr     2000 Hilldrop
Av     1100 Blackmoor
Av     1200 Woodburn
Av     2100 Avondale
Av     2200 Vryheid
Av     3100 Kameelbos
Av     3200 Mafikeng
Gc     1100 Leeupan
Gc     1200 Springvale
Gc     2100 Driehoek
Gc     2200 Croyden
Gc     3100 Draaihoek
Gc     3200 Vlakput
Pn     1100 Goudini
Pn     1200 Jutland
Pn     2100 Ceres
Pn     2200 Reitz
Pn     3100 Mariendal
Pn     3200 Wolwepan
Gf     1100 Woodstock
Gf     1200 Deelspruit
Gf     2100 Maritzdrif
Gf     2200 Braeside
Mp     1100 Hendriksrust
Mp     1200 Kalkfontein
Mp     2100 Omkyk
Mp     2200 Pomfret
Ak     1000 Aroab
Ak     2000 Noenieput
Cv     1100 Twyfelaar
Cv     1200 Brereton
Cv     2100 Buckland
Cv     2200 Leiden
Cv     3100 Setlagole
Cv     3200 Mooilaagte
Bv     1100 Morningside
Bv     1200 Roodebult
Bv     2100 Brandkraal
Bv     2200 Moorfield
Bv     3100 Florida
Bv     3200 Amalia
Bd     1100 Aandrus
Bd     1200 Wilton
Bd     2100 Rietpoort
Bd     2200 Waldo
Bd     3100 Vrede
Bd     3200 Roodeplaat
Ky     1100 Taung
Ky     1200 Riverton
Ky     2100 Nanaga
Ky     2200 Douglas
[Py]   1000 Brakkies
[Py]   2000 Rietrivier
Gr     1000 Nuwerus
Gr     2000 Kotzesrus
Hu     1100 Lillieburn
Hu     1200 Kelvin
Hu     2100 Hayfield
Hu     2200 Suurbekom
Hu     3100 Stella
Hu     3200 Ventersdorp
Sd     1110 Tongaat
Sd     1120 Groothoek
Sd     1210 Empangeni
Sd     1220 Sebati
Sd     2110 Bayala
Sd     2120 Roedtan
Sd     2210 Bolweni
Sd     2220 Zebediela
Sd     3110 Pyramid
Sd     3120 Winfield
Sd     3210 Moletlane
Sd     3220 Steelpoort
Jb     1000 Geelhoutboom
Jb     2000 Tonnelbos
Wf     1100 Applegrove
Wf     1200 Goesabos
Wf     2100 Phantom
Wf     2200 Lebanon
Pg     1000 Highbury
Pg     2000 Ruitersbos
Gk     1100 Bosrug
Gk     1200 Ratelbos
Gk     2100 Noordhoek
Gk     2200 Tierkop
Ss     1100 Smithfield
Ss     1200 Bethulie
Ss     2100 Hermon
Ss     2200 Kareepoort
Se     1110 Rapoeli
Se     1120 Muiskraal
Se     1210 Katdoorn
Se     1220 Ramabesa
Se     2110 Sweethome
Se     2120 Grootvlei
Se     2210 Crondale
Se     2220 Droogpan
Va     1111 Slykspruit
Va     1112 Luckhoff
Va     1121 Goedemoed
Va     1122 Aliwal
Va     1211 Wepener
Va     1212 Dewetsdorp
Va     1221 Helvetia
Va     1222 Serona
Va     2111 Alice
Va     2112 Keiskamma
Va     2121 Homepark
Va     2122 Mastenhof
Va     2211 Meriba
Va     2212 Telpoort
Va     2221 Zuney
Va     2222 Silvervale
Sw     1111 Spreyton
Sw     1112 Breipaal
Sw     1121 Gemvale
Sw     1122 Amandel
Sw     1211 Shangoni
Sw     1212 Burgersdorp
Sw     1221 Mtini
Sw     1222 Rouxville
Sw     2111 Adelaide
Sw     2112 Beaufort
Sw     2121 Riebeeck
Sw     2122 Drew
Sw     2211 Amatole
Sw     2212 Debe
Sw     2221 Bonnievale
Sw     2222 Calvinia
Tu     1110 Entunja
Tu     1120 Olivedale
Tu     1210 Hoeko
Tu     1220 Dikeni
Tu     2110 Mostertshoek
Tu     2120 Scheepersrus
Tu     2210 Amalienstein
Tu     2220 Zandvliet
Et     1111 Platdoorns
Et     1112 Belmont
Et     1121 Oshivelo
Et     1122 Oshakati
Et     1211 Tuli
Et     1212 Kildare
Et     1221 Pontdrif
Et     1222 Tierkloof
Et     2111 Vetkuil
Et     2112 Sannieshof
Et     2121 Namutoni
Et     2122 Ombalantu
Et     2211 Armoed
Et     2212 Migdol
Et     2221 Ulco
Et     2222 Lieberwalt
Gm     1110 Ghaap
Gm     1120 Gletwyn
Gm     1210 Kirkwood
Gm     1220 Merriman
Gm     2110 Vyekraal
Gm     2120 Klondyke
Gm     2210 Deepdrift
Gm     2220 Conway
Ou     1110 Stolsvlakte
Ou     1120 Vleirivier
Ou     1210 Doringbaai
Ou     1220 Highgate
Ou     2110 Dysselsdorp
Ou     2120 Badshoogte
Ou     2210 Volmoed
Ou     2220 Baroe
Oa     1110 Ritchie
Oa     1120 Buchuberg
Oa     1210 Caledon
Oa     1220 Dipene
Oa     2110 Cooper
Oa     2120 Patrysdal
Oa     2210 Gannage
Oa     2220 Rooihoogte
Mu     1110 Baden
Mu     1120 Esperanza
Mu     1210 Scoma
Mu     1220 Rawdon
Mu     2110 Knipes
Mu     2120 Apieshoek
Mu     2210 Trenly
Mu     2220 Rooidam
Ad     1111 Glenconnor
Ad     1112 Dalby
Ad     1121 Centlivres
Ad     1122 Kentvale
Ad     1211 Spekboom
Ad     1212 Gorah
Ad     1221 Walkraal
Ad     1222 Sylvania
Ad     2111 Maurmond
Ad     2112 Airedale
Ad     2121 Felsenheim
Ad     2122 Longhill
Ad     2211 Mimosa
Ad     2212 Peperboom
Ad     2221 Suttondale
Ad     2222 Tregaron
Pr     1110 Hougham
Pr     1120 Ingleside
Pr     1210 Naawte
Pr     1220 Angelierspan
Pr     2110 Downs
Pr     2120 Baarboon
Pr     2210 Verby
Pr     2220 Rooinek
Tr     1110 Dikneus
Tr     1120 Saggiesberg
Tr     1210 Graatjie
Tr     1220 Roelf
Tr     2110 Wolwenes
Tr     2120 Katmakoep
Tr     2210 Perdekraal
Tr     2220 Janrap
Ag     1110 Hefnaar
Ag     1120 Giyani
Ag     1210 Khubus
Ag     1220 Shilowa
Ag     2110 Spoegrivier
Ag     2120 Tankwa
Ag     2210 Landplaas
Ag     2220 Sakrivier
Br     1000 Grootvloer
Br     2000 Kolke
Cg     1000 Nabies
Cg     2000 Marydale
Kn     1000 Bitterfontein
Kn     2000 Kapel
Gs     1111 Dumisa
Gs     1112 Keurkloof
Gs     1121 Kilspindie
Gs     1122 Kammievlei
Gs     1211 Tsende
Gs     1212 Bergsig
Gs     1221 Maringo
Gs     1222 Wheatland
Gs     2111 Overberg
Gs     2112 Inverdoorn
Gs     2121 Botrivier
Gs     2122 Teviot
Gs     2211 Bisho
Gs     2212 Kakamas
Gs     2221 Solitude
Gs     2222 Merwespont
Ms     1100 Myhill
Ms     1200 Carnarvon
Ms     2100 Gulu
Ms     2200 Steinkopf
Du     1110 Nonoti
Du     1120 Sabie
Du     1210 Mtamvuna
Du     1220 Kowie
Du     2110 Marico
Du     2120 Visrivier
Du     2210 Seekoei
Du     2220 Olifants
Nb     1100 Nortier
Nb     1200 Beachwood
Nb     2100 Kalahari
Nb     2200 Henkries
Wb     1000 Thornlea
Wb     2000 Nuwewerf
"

`%||%` <- function(a, b) if (is.null(a)) b else a

criteria <- list(
  list(number = 1L, name = "Fibrous or humified organic material", reads = "the organic O", classes = c("fibrous", "humified"), page = "FC 1"),
  list(number = 2L, name = "Thin or thick humic A", reads = "the humic A", classes = c("thin", "thick"), page = "FC 2"),
  list(number = 3L, name = "Dark or light A over the E (Fernwood)", reads = "the orthic A", classes = c("dark", "light"), page = "FC 3"),
  list(number = 4L, name = "Bleached orthic A", reads = "the orthic A", classes = c("bleached", "not_bleached"), page = "FC 4"),
  list(number = 5L, name = "Dark, red or other colour (vertic A; pedocutanic B under a melanic A)", reads = "the vertic A or the pedocutanic B",
       classes = c("dark", "red", "other"), page = "FC 5"),
  list(number = 6L, name = "Grey or yellow E", reads = "the E", classes = c("grey", "yellow"), page = "FC 6"),
  list(number = 7L, name = "Lamellae in the E (Fernwood)", reads = "the E", classes = c("absent", "present"), page = "FC 7"),
  list(number = 8L, name = "Base status of the B1: dystrophic, mesotrophic, eutrophic", reads = "the B1",
       classes = c("dystrophic", "mesotrophic", "eutrophic"), page = "FC 8"),
  list(number = 9L, name = "Red or non-red B, stratified alluvium or regic sand", reads = "the B, the alluvium or the regic sand",
       classes = c("non_red", "red"), page = "FC 9"),
  list(number = 10L, name = "Luvic B1", reads = "the B1 against the A or E directly above", classes = c("non_luvic", "luvic"), page = "FC 10"),
  list(number = 11L, name = "Subangular or fine angular against medium or coarse angular structure", reads = "the pedocutanic or red structured B",
       classes = c("subangular_fine", "medium_coarse"), page = "FC 11"),
  list(number = 12L, name = "Continuous black cutans in the prismacutanic B (Estcourt)", reads = "the prismacutanic B",
       classes = c("without", "with"), page = "FC 12"),
  list(number = 13L, name = "Ortstein in the podzol B", reads = "the podzol B", classes = c("not_hardened", "hardened"), page = "FC 13"),
  list(number = 14L, name = "Hard or not hard lithocutanic B or saprolite", reads = "the lithocutanic B1 or the upper saprolite",
       classes = c("not_hard", "hard"), page = "FC 14"),
  list(number = 15L, name = "Signs of wetness", reads = "the horizon the family names", classes = c("absent", "present"), page = "FC 15"),
  list(number = 16L, name = "Calcareous", reads = "the horizon the family names", classes = c("non_calcareous", "calcareous"), page = "FC 16"),
  list(number = 17L, name = "Podzolic character below the yellow-brown apedal B (Constantia)", reads = "the layers below the yellow-brown apedal B",
       classes = c("absent", "present"), page = "FC 17"),
  list(number = 18L, name = "Friable or firm C", reads = "the upper C", classes = c("friable", "firm"), page = "FC 18"),
  list(number = 19L, name = "Material under the organic O", reads = "the layers below the O",
       classes = c("rock_or_saprolite", "unconsolidated"), page = "FC 19"))

# the K page of each form, from key.yaml
key <- yaml::read_yaml("inst/rules/za1991/key.yaml")$forms
kpage <- vapply(key, function(f) {
  p <- trimws(strsplit(f$pages, ";", fixed = TRUE)[[1]])
  p <- p[startsWith(p, "K ")]
  if (length(p)) p[1L] else NA_character_
}, "")
names(kpage) <- vapply(key, `[[`, "", "code")

rd <- function(txt) {
  l <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1]])
  l[nzchar(l)]
}
cond <- function(s) {
  m <- regmatches(s, regexec("^([0-9]+):([A-Za-z_0-9]+):([A-Z-]+)=([a-z_|]+)$", trimws(s)))[[1]]
  if (length(m) != 5L) stop("bad condition: ", s)
  cl <- strsplit(m[5], "|", fixed = TRUE)[[1]]
  crit <- criteria[[as.integer(m[2])]]
  if (!all(cl %in% crit$classes)) stop("class not of criterion ", m[2], ": ", s)
  list(criterion = as.integer(m[2]), part = m[3], horizon = m[4], class = if (length(cl) == 1L) cl else as.list(cl))
}
dg <- lapply(rd(digits), function(l) {
  m <- regmatches(l, regexec("^(\\S+)\\s+([0-9])\\s+(.*)$", l))[[1]]
  list(form = m[2], position = as.integer(m[3]),
       classes = lapply(strsplit(m[4], " ; ", fixed = TRUE)[[1]], function(cl) lapply(strsplit(cl, " & ", fixed = TRUE)[[1]], cond)))
})
fm <- lapply(rd(families), function(l) {
  m <- regmatches(l, regexec("^(\\S+)\\s+([0-9]{4})\\s+(.+)$", l))[[1]]
  list(form = m[2], digits = m[3], name = m[4])
})
forms <- unique(vapply(fm, `[[`, "", "form"))
stopifnot(length(fm) == 404L, length(forms) == 73L, all(forms %in% names(kpage)))

notes <- function(f, d) {
  n <- character(0)
  if (f == "[Py]") n <- c(n, "code read 1000 or 2000 where the key page prints three digits (plan section 10, slip 1)")
  if (f == "Sd" && startsWith(d, "3")) n <- c(n, "the luvic criterion read on the B1, as in the other Shortlands families (plan section 10, slip 6)")
  if (f %in% c("Et", "Ad", "Br")) n <- c(n, "signs of wetness in the carbonate horizon read on the soft carbonate horizon (FC 15; plan section 10, slip 7)")
  if (f %in% c("Va", "Sw", "Et", "Ad")) n <- c(n, "a class header of the key page printed 1110 (2110> in Swartland) read 1100 (2110); the family codes are as printed (plan section 10, slips 2 and 3)")
  if (length(n)) paste(n, collapse = "; ") else NULL
}

out_fam <- list(); out_dig <- list()
for (f in forms) {
  ds <- Filter(function(z) z$form == f, dg)
  ds <- ds[order(vapply(ds, `[[`, 0L, "position"))]
  stopifnot(identical(vapply(ds, `[[`, 0L, "position"), seq_along(ds)))
  out_dig[[f]] <- lapply(ds, function(z) list(position = z$position, classes = z$classes))
  ff <- Filter(function(z) z$form == f, fm)
  out_fam[[f]] <- lapply(ff, function(z) {
    dd <- as.integer(strsplit(z$digits, "")[[1]])
    stopifnot(all(dd[seq_along(ds)] >= 1L), all(dd[-seq_along(ds)] == 0L))
    tests <- unlist(lapply(seq_along(ds), function(p) ds[[p]]$classes[[dd[p]]]), recursive = FALSE)
    e <- list(code = paste0(f, z$digits), name = z$name, digits = z$digits, page = unname(kpage[f]))
    nt <- notes(f, z$digits)
    if (!is.null(nt)) e$note <- nt
    e$tests <- list(all_of = lapply(tests, function(t) list(family_class_za = t)))
    e
  })
}

head <- c(
  "# The 404 families of the 73 soil forms of the South African soil",
  "# classification of 1991 (Soil Classification Working Group, 1991), and its",
  "# 19 family criteria. Written by data-raw/za1991_families.R; edit there.",
  "#",
  "#   family_criteria  the 19 criteria (FC), soilKey's names and class keys",
  "#                    (paraphrased; no text of the sources)",
  "#   family_digits    per form, the digits of the family code in order: the",
  "#                    conditions of each class (1, 2, 3 ...), for the partial",
  "#                    code of an undecided family (Hu1x00)",
  "#   families         per form code, its families in the order of the key:",
  "#                    code (form code and the four printed digits), name as",
  "#                    printed, the K page, and the tests: all_of the",
  "#                    family_class_za() conditions of its digits",
  "")
yaml::write_yaml(list(edition_id = "za1991", family_criteria = criteria, family_digits = out_dig, families = out_fam),
                 "inst/rules/za1991/families.yaml.tmp")
body <- readLines("inst/rules/za1991/families.yaml.tmp")
unlink("inst/rules/za1991/families.yaml.tmp")
writeLines(c(head, body), "inst/rules/za1991/families.yaml")
cat("wrote", sum(lengths(out_fam)), "families of", length(out_fam), "forms\n")
