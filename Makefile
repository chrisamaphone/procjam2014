
.PSEUDO: default

default: tamaraify

clftrace.lex.sml: clftrace.lex
	mllex clftrace.lex

clftrace.grm.sml: clftrace.grm
	mlyacc clftrace.grm

tamaraify: sources.mlb clftrace.lex.sml clftrace.grm.sml *.sml
	mlton -const "Exn.keepHistory true" -output tamaraify sources.mlb
