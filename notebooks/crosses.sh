rm crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-abs -max-cost 9 -csv ../bench/cad2/crosses_5 >> crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-abs -max-cost 9 -csv ../bench/cad2/crosses_10 >> crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-abs -max-cost 9 -csv ../bench/cad2/crosses_20 >> crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-concrete -max-cost 9 -csv ../bench/cad2/crosses_5 >> crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-concrete -max-cost 9 -csv ../bench/cad2/crosses_10 >> crosses.csv
dune exec ../bin/lazy_cegis.exe -- cad-concrete -max-cost 9 -csv ../bench/cad2/crosses_20 >> crosses.csv
cat crosses.csv
