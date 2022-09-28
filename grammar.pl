% Example queries:
% | ?- utterance('example',S,['D'('A'),'N'(man),'VBP;~I'(smiles),'PU'('.')],[]).
% | ?- tphrase_set_string(['D'('A'),'N'('man'),'VBP;~I'(smiles),'PU'('.')]), tphrase(utterance('example',X)).

:- import member/2 from basics.
:- import gensym/2 from gensym.

:- auto_table.

% Sentence

utterance(Id,n(Id,IP,none)) -->
  clause_top_layer(statement,[],IPL-L1),
  punc(final,L1-[]),
  {
    IP =.. ['IP-MAT'|IPL]
  }.
utterance(Id,n(Id,IP,none)) -->
  verb_phrase_layer([],filled_subject,imperative,active,IPL-L1),
  punc(final,L1-[]),
  {
    IP =.. ['IP-IMP'|IPL]
  }.
utterance(Id,n(Id,'CP-QUE'(IP,PU),none)) -->
  clause_top_layer(matrix_interrogative,[],IPL-[]),
  punc(final_question,[PU]-[]),
  {
    IP =.. ['IP-SUB'|IPL]
  }.
utterance(Id,n(Id,IP,none)) -->
  clause_top_layer(_Type,[],IPL-[]),
  {
    IP =.. ['IML'|IPL]
  }.
utterance(Id,n(Id,'FRAG'(NP),none)) -->
  noun_phrase('',[NP]-[]).
utterance(Id,n(Id,'FRAG'(ADJP),none)) -->
  adjective_phrase('',[ADJP]-[]).
utterance(Id,n(Id,'FRAG'(Phrase),none)) -->
  adverbial([Phrase]-[]).

% Nouns

noun(['NS'(Word)|L]-L) --> ['NS'(Word)].
noun(['N'(Word)|L]-L) --> ['N'(Word)].

noun_head(['NPRS'(Word)|L]-L) --> ['NPRS'(Word)].
noun_head(['NPR'(Word)|L]-L) --> ['NPR'(Word)].
noun_head(['D;_nphd_'(Word)|L]-L) --> ['D;_nphd_'(Word)].
noun_head(['Q;_nphd_'(Word)|L]-L) --> ['Q;_nphd_'(Word)].

noun_head_full(['PNX'(Word)|L]-L) --> ['PNX'(Word)].
noun_head_full(['PRO'(Word)|L]-L) --> ['PRO'(Word)].
noun_head_full(['NP-GENV'('PRO;_ppge_'(Word))|L]-L) --> ['PRO;_ppge_'(Word)].
noun_head_full(['WPRO'(Word)|L]-L) --> ['WPRO'(Word)].

relative_noun_head_full(['RPRO'(Word)|L]-L) --> ['RPRO'(Word)].

% Determiners

det(['D'(Word)|L]-L) --> ['D'(Word)].
det(['WD'(Word)|L]-L) --> ['WD'(Word)].
det(['Q'(Word)|L]-L) --> ['Q'(Word)].

% Genitive words

genm(['GENM'(Word)|L]-L) --> ['GENM'(Word)].

pronoun_genm(['PRO;_genm_'(Word)|L]-L) --> ['PRO;_genm_'(Word)].
pronoun_genm(['WPRO;_genm_'(Word)|L]-L) --> ['WPRO;_genm_'(Word)].

% Clause subject

subject(cleft,['NP-PSBJ'('PRO;_clf_'(Word))|L]-L) -->
  ['PRO;_clf_'(Word)].
subject(provisional,['NP-PSBJ'('PRO;_pvl_'(Word))|L]-L) -->
  ['PRO;_pvl_'(Word)].
subject(there,['EX'(Word)|L]-L) -->
  ['EX'(Word)].
subject(filled_subject,L) -->
  noun_phrase('-SBJ',L).

% Adverbs

adv(['ADV'(Word)|L]-L) --> ['ADV'(Word)].
adv(['ADVR'(Word)|L]-L) --> ['ADVR'(Word)].
adv(['ADVS'(Word)|L]-L) --> ['ADVS'(Word)].
adv(['WADV'(Word)|L]-L) --> ['WADV'(Word)].
adv(['RP'(Word)|L]-L) --> ['RP'(Word)].
adv_catenative(['ADV;_cat_'(Word)|L]-L) --> ['ADV;_cat_'(Word)].

% Adjectives

adj(['ADJ'(Word)|L]-L) --> ['ADJ'(Word)].
adj(['ADJR'(Word)|L]-L) --> ['ADJR'(Word)].
adj(['ADJS'(Word)|L]-L) --> ['ADJS'(Word)].
adj_catenative(['ADJ;_cat_'(Word)|L]-L) --> ['ADJ;_cat_'(Word)].

% Verb word construction

verb(Tag,Code,[TagCodeWord|L]-L) -->
  [TagCodeWord],
  {
    atom_concat(Tag,Code,TagCode),
    TagCodeWord =.. [TagCode,Word]
  }.

verb_with_complement(Displaced,Sbj_type,Infl,Voice,L-L0) -->
  {
    verb_tag(Infl,Tag_list),
    member(Tag,Tag_list),
    sub_atom(Tag,0,1,_,C),
    verb_code(C,Code)
  },
  verb(Tag,Code,L-L1),
  verb_complement(Tag,Code,Displaced,Sbj_type,Voice,L1-L0).

% Verb tags

verb_tag(finite,['VBP','VBD','DOP','DOD','HVP','HVD','BEP','BED']).
verb_tag(imperative,['VB','DO','HV','BE']).
verb_tag(infinitive,['VB','DO','HV','BE']).
verb_tag(ing_participle,['VAG','DAG','HAG','BAG']).
verb_tag(en_participle,['VVN','DON','HVN','BEN']).

% Verb codes

verb_code('H',Code) :-
  member(Code,[';~Tn',';~cat_Vt',';~cat_Ve']).
verb_code('B',Code) :-
  member(Code,[
     ';~La',';~Ln',
     ';~I',';~Ip',';~Ipr',
     ';~cat_Vt',';~cat_Vg',';~cat_Ve',
     ';~ex_V',';~ex_Vp',';~ex_Vpr',';~ex_cat_Vt',';~ex_cat_Vg',';~ex_cat_Ve',
     ';~equ_Vf',';~equ_Vw',';~equ_Vt',';~equ_Vg',
     ';~cleft_Vn'
    ]).
verb_code('D',Code) :-
  member(Code,[';~I',';~Tn']).
verb_code('V',Code) :-
  member(Code,[
     ';~La',';~Ln',
     ';~I',';~Ip',';~Ipr',';~It',
     ';~Tn',';~Tn.p',';~Tn.pr',';~Tf',';~Tw',';~Tt',';~Tnt',';~Tni',';~Tg',';~Tng',
     ';~Dn.n',';~Dn.pr',';~Dn.f',';~Dn.w',';~Dn.t',
     ';~Cn.a',';~Cn.n',';~Cn.t',';~Cn.i',';~Cn.g',
     ';~cat_Vt',';~cat_Vg',';~cat_Ve'
    ]).

verb_complement(Tag,Code,Displaced,provisional,Voice,L-L0) -->
  verb_complement(Tag,Code,Displaced,filled_subject,Voice,L-L1),
  notional_subject(L1-L0).

% Linking verb complements

verb_complement(_Tag,';~La',[],filled_subject,active,L) -->
  adjective_phrase('-PRD2',L).

verb_complement(_Tag,';~Ln',[],filled_subject,active,L) -->
  noun_phrase('-PRD2',L).
verb_complement(_Tag,';~Ln',[np(ICH)],filled_subject,active,['NP-PRD2'(ICH)|L]-L) -->
  [].

% Intransitive verb complements

verb_complement(_Tag,';~I',[],filled_subject,active,L-L) -->
  [].

verb_complement(_Tag,';~Ip',[],filled_subject,active,L) -->
  adverb_phrase(L).

verb_complement(_Tag,';~Ipr',[],filled_subject,active,L) -->
  preposition_phrase('',L).
verb_complement(_Tag,';~Ipr',[np(ICH)],filled_subject,active,
['PP'(Role,'NP'(ICH))|L]-L) -->
  role([Role]-[]).
verb_complement(_Tag,';~Ipr',[pp(ICH)],filled_subject,active,['PP'(ICH)|L]-L) -->
  [].

verb_complement(_Tag,';~It',[],filled_subject,active,L) -->
  ip_to_inf('-OB1',Displaced,_Sbj_type,L).

% Mono-transitive verb complements

verb_complement(_Tag,';~Tn',[],filled_subject,active,L) -->
  noun_phrase('-OB1',L).
verb_complement(_Tag,';~Tn',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L) -->
  [].
verb_complement(_Tag,';~Tn',[],filled_subject,passive,L-L) -->
  [].

verb_complement(_Tag,';~Tn.p',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  adverb_phrase(L1-L0).
verb_complement(_Tag,';~Tn.p',[],filled_subject,active,L-L0) -->
  adverb_phrase(L-L1),
  noun_phrase('-OB1',L1-L0).
verb_complement(_Tag,';~Tn.p',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  adverb_phrase(L-L0).
verb_complement(_Tag,';~Tn.p',[],filled_subject,passive,L) -->
  adverb_phrase(L).

verb_complement(_Tag,';~Tn.pr',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  preposition_phrase('',L1-L0).
verb_complement(_Tag,';~Tn.pr',[np(ICH)],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-['PP'(Role,'NP'(ICH))|L0]),
  role([Role]-[]).
verb_complement(_Tag,';~Tn.pr',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  preposition_phrase('',L-L0).
verb_complement(_Tag,';~Tn.pr',[],filled_subject,passive,L) -->
  preposition_phrase('',L).

verb_complement(_Tag,';~Tf',Displaced,filled_subject,active,L) -->
  cp_that('-OB1',Displaced,L).

verb_complement(_Tag,';~Tw',Displaced,filled_subject,active,L) -->
  cp_que('-OB1',Displaced,L).

verb_complement(_Tag,';~Tt',Displaced,filled_subject,active,L) -->
  ip_to_inf('-OB1',Displaced,_Sbj_type,L).

verb_complement(_Tag,';~Tnt',[],filled_subject,active,L-L0) -->
  noun_phrase('-DOB1',L-L1),
  ip_to_inf('-OB1',[],filled_subject,L1-L0).
verb_complement(_Tag,';~Tnt',[np(ICH)],filled_subject,active,
['NP-DOB1'(ICH)|L]-L0) -->
  ip_to_inf('-OB1',[],filled_subject,L-L0).
verb_complement(_Tag,';~Tnt',[],filled_subject,passive,L) -->
  ip_to_inf('-OB1',[],filled_subject,L).

verb_complement(_Tag,';~Tni',[],filled_subject,active,L-L0) -->
  noun_phrase('-DOB1',L-[IP|L0]),
  verb_phrase_layer(Displaced,filled_subject,infinitive,active,VPL-[]),
  {
    IP =.. ['IP-INF-OB1'|VPL]
  }.
verb_complement(_Tag,';~Tni',[np(ICH)],filled_subject,active,
['NP-DOB1'(ICH),IP|L]-L) -->
  verb_phrase_layer(Displaced,filled_subject,infinitive,active,VPL-[]),
  {
    IP =.. ['IP-INF-OB1'|VPL]
  }.

verb_complement(_Tag,';~Tg',Displaced,filled_subject,active,L) -->
  ip_ppl('-OB1',Displaced,filled_subject,ing_participle,L).

verb_complement(_Tag,';~Tng',[],filled_subject,active,L-L0) -->
  noun_phrase('-DOB1',L-L1),
  ip_ppl('-OB1',[],filled_subject,ing_participle,L1-L0).
verb_complement(_Tag,';~Tng',[np(ICH)],filled_subject,active,
['NP-DOB1'(ICH)|L]-L0) -->
  ip_ppl('-OB1',[],filled_subject,ing_participle,L-L0).
verb_complement(_Tag,';~Tng',[],filled_subject,passive,L) -->
  ip_ppl('-OB1',[],filled_subject,ing_participle,L).

% Ditransitive verb complements

verb_complement(_Tag,';~Dn.n',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB2',L-L1),
  noun_phrase('-OB1',L1-L0).
verb_complement(_Tag,';~Dn.n',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  noun_phrase('-OB2',L-L0).
verb_complement(_Tag,';~Dn.n',[np(ICH)],filled_subject,active,
['NP-OB2'(ICH)|L]-L0) -->
  noun_phrase('-OB1',L-L0).
verb_complement(_Tag,';~Dn.n',[],filled_subject,passive,L) -->
  noun_phrase('-OB1',L).
verb_complement(_Tag,';~Dn.n',[np(ICH)],filled_subject,passive,
['NP-OB1'(ICH)|L]-L) -->
  [].

verb_complement(_Tag,';~Dn.pr',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  preposition_phrase('-OBU',L1-L0).
verb_complement(_Tag,';~Dn.pr',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  preposition_phrase('-OBU',L-L0).
verb_complement(_Tag,';~Dn.pr',[],filled_subject,passive,L) -->
  preposition_phrase('-OBU',L).

verb_complement(_Tag,';~Dn.f',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB2',L-L1),
  cp_that('-OB1',Displaced,L1-L0).
verb_complement(_Tag,';~Dn.f',Displaced,filled_subject,passive,L) -->
  cp_that('-OB1',Displaced,L).

verb_complement(_Tag,';~Dn.w',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB2',L-L1),
  cp_que('-OB1',Displaced,L1-L0).
verb_complement(_Tag,';~Dn.w',Displaced,filled_subject,passive,L) -->
  cp_que('-OB1',Displaced,L).

verb_complement(_Tag,';~Dn.t',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB2',L-L1),
  ip_to_inf('-OB1',Displaced,filled_subject,L1-L0).
verb_complement(_Tag,';~Dn.t',Displaced,filled_subject,passive,L) -->
  ip_to_inf('-OB1',Displaced,filled_subject,L).

% Complex-transitive verb complements

verb_complement(_Tag,';~Cn.a',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  adjective_phrase('-PRD',L1-L0).
verb_complement(_Tag,';~Cn.a',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  adjective_phrase('-PRD',L-L0).
verb_complement(_Tag,';~Cn.a',[],filled_subject,passive,L) -->
  adjective_phrase('-PRD',L).

verb_complement(_Tag,';~Cn.n',[],filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  noun_phrase('-PRD',L1-L0).
verb_complement(_Tag,';~Cn.n',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  noun_phrase('-PRD',L-L0).
verb_complement(_Tag,';~Cn.n',[],filled_subject,passive,L) -->
  noun_phrase('-PRD',L).

verb_complement(_Tag,';~Cn.t',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  ip_to_inf('-PRD',Displaced,filled_subject,L1-L0).
verb_complement(_Tag,';~Cn.t',[np(ICH)],filled_subject,active,
['NP-OB1'(ICH)|L]-L0) -->
  ip_to_inf('-PRD',[],filled_subject,L-L0).
verb_complement(_Tag,';~Cn.t',Displaced,filled_subject,passive,L) -->
  ip_to_inf('-PRD',Displaced,filled_subject,L).

verb_complement(_Tag,';~Cn.i',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-[IP|L0]),
  verb_phrase_layer(Displaced,filled_subject,infinitive,active,VPL-[]),
  {
    IP =.. ['IP-INF-PRD'|VPL]
  }.

verb_complement(_Tag,';~Cn.g',Displaced,filled_subject,active,L-L0) -->
  noun_phrase('-OB1',L-L1),
  ip_ppl('-PRD',Displaced,filled_subject,ing_participle,L1-L0).
verb_complement(_Tag,';~Cn.g',Displaced,filled_subject,passive,L) -->
  ip_ppl('-PRD',Displaced,filled_subject,ing_participle,L).

% Catenative verb complements

verb_complement(_Tag,';~cat_Vt',Displaced,Sbj_type,active,L) -->
  ip_to_inf('-CAT',Displaced,Sbj_type,L).

verb_complement(_Tag,';~cat_Vg',Displaced,Sbj_type,active,L) -->
  ip_ppl('-CAT',Displaced,Sbj_type,ing_participle,L).

verb_complement(Tag,';~cat_Ve',Displaced,Sbj_type,active,L) -->
  {
    member(Tag,['HVP','HVD','HV','HAG','HVN'])
  },
  ip_ppl('-CAT',Displaced,Sbj_type,en_participle,L).
verb_complement(Tag,';~cat_Ve',Displaced,Sbj_type,active,L) -->
  {
    \+ member(Tag,['HVP','HVD','HV','HAG','HVN'])
  },
  ip_ppl_passive('-CAT',Displaced,Sbj_type,L).

% Existential verb complements

verb_complement(_Tag,';~ex_V',[],there,active,L) -->
  noun_phrase('-ESBJ',L).
verb_complement(_Tag,';~ex_V',[np(ICH)],there,active,['NP-ESBJ'(ICH)|L]-L) -->
  [].

verb_complement(_Tag,';~ex_Vp',[],there,active,L-L0) -->
  noun_phrase('-ESBJ',L-L1),
  adverb_phrase(L1-L0).
verb_complement(_Tag,';~ex_Vp',[np(ICH)],there,active,['NP-ESBJ'(ICH)|L]-L0) -->
  adverb_phrase(L-L0).

verb_complement(_Tag,';~ex_Vpr',[],there,active,L-L0) -->
  noun_phrase('-ESBJ',L-L1),
  preposition_phrase('',L1-L0).
verb_complement(_Tag,';~ex_Vpr',[np(ICH)],there,active,['NP-ESBJ'(ICH)|L]-L0) -->
  preposition_phrase('',L-L0).
verb_complement(_Tag,';~ex_Vpr',[pp(ICH)],there,active,L-L0) -->
  noun_phrase('-ESBJ',L-['PP'(ICH)|L0]).
verb_complement(_Tag,';~ex_Vpr',[np(ICH2),pp(ICH1)],there,active,
['NP-ESBJ'(ICH2),'PP'(ICH1)|L]-L) -->
  [].

verb_complement(_Tag,';~ex_cat_Vt',Displaced,there,active,L-L0) -->
  noun_phrase('-ESBJ',L-L1),
  ip_to_inf('-CAT',Displaced,filled_subject,L1-L0).

verb_complement(_Tag,';~ex_cat_Vg',Displaced,there,active,L-L0) -->
  noun_phrase('-ESBJ',L-L1),
  ip_ppl('-CAT',Displaced,filled_subject,ing_participle,L1-L0).

verb_complement(_Tag,';~ex_cat_Ve',Displaced,there,active,L-L0) -->
  noun_phrase('-ESBJ',L-L1),
  ip_ppl_passive('-CAT',Displaced,filled_subject,L1-L0).

% Equatives

verb_complement(_Tag,';~equ_Vf',[],filled_subject,active,L) -->
  cp_that('-PRD2',Displaced,L).

verb_complement(_Tag,';~equ_Vw',[],filled_subject,active,L) -->
  cp_que('-PRD2',Displaced,L).

verb_complement(_Tag,';~equ_Vt',[],filled_subject,active,L) -->
  ip_to_inf('-PRD2',[],_Sbj_type,L).

verb_complement(_Tag,';~equ_Vg',[],filled_subject,active,L) -->
  ip_ppl('-PRD2',[],filled_subject,ing_participle,L).

% Cleft

verb_complement(_Tag,';~cleft_Vn',[],cleft,active,L-L0) -->
  noun_phrase('-FOC',L-[IP|L0]),
  relative_clause_top_layer(IPL-[]),
  {
    IP =.. ['IP-CLF'|IPL]
  }.

% Modal words

modal(['MD'(Word)|L]-L) --> ['MD'(Word)].
modal_catenative(['MD;~cat_Vt'(Word)|L]-L) --> ['MD;~cat_Vt'(Word)].

% Other clause level words

neg(['NEG'(Word)|L]-L) --> ['NEG'(Word)].
to(['TO'(Word)|L]-L) --> ['TO'(Word)].

% Connective words

conj('CONJ'(Word)) --> ['CONJ'(Word)].
comp(['C'(Word)|L]-L) --> ['C'(Word)].
comp_wq(['WQ'(Word)|L]-L) --> ['WQ'(Word)].
conn(['P-CONN'(Word)|L]-L) --> ['P-CONN'(Word)].
role(['P-ROLE'(Word)|L]-L) --> ['P-ROLE'(Word)].

% Punctuation

punc(final_question,['PU'('?')|L]-L) --> ['PU'('?')].
punc(final,['PU'('.')|L]-L) --> ['PU'('.')].
punc(non_final,['PU'(',')|L]-L) --> ['PU'(',')].

% Noun phrase

noun_phrase(Ext,[NP|L]-L) -->
  noun_phrase_top(NPL-[]),
  {
    atom_concat('NP',Ext,Label),
    NP =.. [Label|NPL]
  }.

noun_phrase_top(L) -->
  noun_phrase_initial_layer(L).
noun_phrase_top(L) -->
  noun_head_full(L).

noun_phrase_initial_layer(L-L0) -->
  determiner_layer(L-L1),
  internal_np_higher_layer(L1-L0).
noun_phrase_initial_layer(L) -->
  noun_head(L).
noun_phrase_initial_layer(L) -->
  internal_np_higher_layer(L).
noun_phrase_initial_layer([NML|L]-L) -->
  noun_phrase('',[NP]-[]),
  noun_phrase_initial_tail(L1-[]),
  {
    NML =.. ['NML',NP|L1]
  }.
noun_phrase_initial_layer(L-L0) -->
  noun_phrase_initial_layer(L-L1),
  preposition_phrase('',L1-L0).
noun_phrase_initial_layer(L-L0) -->
  noun_phrase_initial_layer(L-L1),
  relative_clause(L1-L0).

noun_phrase_initial_tail(['CONJP'(CONJ,NP)|L]-L) -->
  conj(CONJ),
  noun_phrase('',[NP]-[]).
noun_phrase_initial_tail([PU,'CONJP'(NP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  noun_phrase('',[NP]-[]),
  noun_phrase_initial_tail(L-L0).

internal_np_higher_layer(L-L0) -->
  adjective_phrase('',L-L1),
  internal_np_higher_layer(L1-L0).
internal_np_higher_layer(L) -->
  internal_np_lower_layer(L).
internal_np_higher_layer(L-L0) -->
  internal_np_higher_layer(L-L1),
  preposition_phrase('',L1-L0).
internal_np_higher_layer(L-L0) -->
  internal_np_higher_layer(L-L1),
  relative_clause(L1-L0).

internal_np_lower_layer(L) -->
  noun(L).
internal_np_lower_layer(L-L0) -->
  noun(L-L1),
  internal_np_lower_layer(L1-L0).
internal_np_lower_layer([NML|L]-L) -->
  internal_np_lower_layer(NPL-[]),
  internal_np_lower_tail(L1-[]),
  {
    NP =.. ['NP'|NPL],
    NML =.. ['NML',NP|L1]
  }.

internal_np_lower_tail(['CONJP'(CONJ,NP)|L]-L) -->
  conj(CONJ),
  internal_np_lower_layer(NPL-[]),
  {
    NP =.. ['NP'|NPL]
  }.
internal_np_lower_tail([PU,'CONJP'(NP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  internal_np_lower_layer(NPL-[]),
  internal_np_lower_tail(L-L0),
  {
    NP =.. ['NP'|NPL]
  }.

% Determiner layer

determiner_layer(L) -->
  det(L).
determiner_layer([NP|L]-L) -->
  noun_phrase_genm_layer(NPL-[]),
  {
    NP =.. ['NP-GENV'|NPL]
  }.

% Genitive noun phrases

noun_phrase_genm_layer(L) -->
  pronoun_genm(L).
noun_phrase_genm_layer(L-L0) -->
  noun_phrase_initial_layer(L-L1),
  genm(L1-L0).
noun_phrase_genm_layer([NML|L]-L) -->
  noun_phrase_genm_layer(NPL-[]),
  noun_phrase_genm_tail(L1-[]),
  {
    NP =.. ['NP'|NPL],
    NML =.. ['NML',NP|L1]
  }.

noun_phrase_genm_tail(['CONJP'(CONJ,NP)|L]-L) -->
  conj(CONJ),
  noun_phrase_genm_layer(NPL-[]),
  {
    NP =.. ['NP'|NPL]
  }.
noun_phrase_genm_tail([PU,'CONJP'(NP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  noun_phrase_genm_layer(NPL-[]),
  noun_phrase_genm_tail(L-L0),
  {
    NP =.. ['NP'|NPL]
  }.

% Adverb phrase

adverb_phrase([ADVP|L]-L) -->
  adverb_phrase_layer(ADVPL-[]),
  {
    ADVP =.. ['ADVP'|ADVPL]
  }.

adverb_phrase_layer(L) -->
  adv(L).
adverb_phrase_layer(L-L0) -->
  adv_catenative(L-L1),
  ip_to_inf('',[],filled_subject,L1-L0).
adverb_phrase_layer(L-L0) -->
  adverb_phrase(L-L1),
  adverb_phrase_layer(L1-L0).
adverb_phrase_layer(L-L0) -->
  adverb_phrase_layer(L-L1),
  preposition_phrase('',L1-L0).
adverb_phrase_layer([AML|L]-L) -->
  adverb_phrase([ADVP]-[]),
  adverb_phrase_tail(L1-[]),
  {
    AML =.. ['AML',ADVP|L1]
  }.

adverb_phrase_tail(['CONJP'(CONJ,ADVP)|L]-L) -->
  conj(CONJ),
  adverb_phrase([ADVP]-[]).
adverb_phrase_tail([PU,'CONJP'(ADVP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  adverb_phrase([ADVP]-[]),
  adverb_phrase_tail(L-L0).

% Adjective phrase

adjective_phrase(Ext,[ADJP|L]-L) -->
  adjective_phrase_layer(ADJPL-[]),
  {
    atom_concat('ADJP',Ext,Label),
    ADJP =.. [Label|ADJPL]
  }.

adjective_phrase_layer(L) -->
  adj(L).
adjective_phrase_layer(L-L0) -->
  adj_catenative(L-L1),
  ip_to_inf('',[],filled_subject,L1-L0).
adjective_phrase_layer(L-L0) -->
  adverb_phrase(L-L1),
  adjective_phrase_layer(L1-L0).
adjective_phrase_layer(L-L0) -->
  adjective_phrase_layer(L-L1),
  preposition_phrase('',L1-L0).
adjective_phrase_layer([AML|L]-L) -->
  adjective_phrase('',[ADJP]-[]),
  adjective_phrase_tail(L1-[]),
  {
    AML =.. ['AML',ADJP|L1]
  }.

adjective_phrase_tail(['CONJP'(CONJ,ADJP)|L]-L) -->
  conj(CONJ),
  adjective_phrase('',[ADJP]-[]).
adjective_phrase_tail([PU,'CONJP'(ADJP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  adjective_phrase('',[ADJP]-[]),
  adjective_phrase_tail(L-L0).

% Preposition phrases

preposition_phrase(Ext,[PP|L]-L) -->
  preposition_phrase_layer(PPL-[]),
  {
    atom_concat('PP',Ext,Label),
    PP =.. [Label|PPL]
  }.
preposition_phrase(Ext,[PP|L]-L) -->
  preposition_phrase('',[PP1]-[]),
  preposition_phrase_tail(L1-[]),
  {
    atom_concat('PP',Ext,Label),
    PP =.. [Label,PP1|L1]
  }.

preposition_phrase_layer(L-L0) -->
  role(L-L1),
  noun_phrase('',L1-L0).

preposition_phrase_tail(['CONJP'(CONJ,PP)|L]-L) -->
  conj(CONJ),
  preposition_phrase('',[PP]-[]).
preposition_phrase_tail([PU,'CONJP'(PP)|L]-L0) -->
  punc(non_final,[PU]-[]),
  preposition_phrase('',[PP]-[]),
  preposition_phrase_tail(L-L0).

% Optional non-final punctuation

optional_non_final_punc(L) -->
  punc(non_final,L).
optional_non_final_punc(L-L) -->
  [].

% Clause top layer

clause_top_layer(_Type,Displaced,L-L0) -->
  subject(Sbj_type,L-L1),
  verb_phrase_layer(Displaced,Sbj_type,finite,active,L1-L0).
clause_top_layer(_Type,Displaced,L-L0) -->
  subject(Sbj_type,L-L1),
  clause_middle_layer(Displaced,Sbj_type,L1-L0).
clause_top_layer(matrix_interrogative,Displaced,L-L0) -->
  operator_layer(L-L2),
  subject(Sbj_type,L2-L1),
  verb_phrase_layer(Displaced,Sbj_type,infinitive,active,L1-L0).
clause_top_layer(matrix_interrogative,Displaced,L-L0) -->
  have_be_or_mod_cat_finite_layer(Tag,Code,L-L2),
  subject(Sbj_type,L2-L1),
  verb_complement(Tag,Code,Displaced,Sbj_type,active,L1-L0).
clause_top_layer(Type,Displaced,L-L0) -->
  adverbial(L-L2),
  optional_non_final_punc(L2-L1),
  clause_top_layer(Type,Displaced,L1-L0).
clause_top_layer(Type,Displaced,L-L0) -->
  clause_top_layer(Type,Displaced,L-L2),
  optional_non_final_punc(L2-L1),
  adverbial(L1-L0).
clause_top_layer(Type,Displaced,L-L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,L-L2),
  optional_non_final_punc(L2-L1),
  clause_top_layer(Type,[np(ICH)|Displaced],L1-L0).
clause_top_layer(Type,Displaced,L-L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  preposition_phrase(Index,L-L2),
  optional_non_final_punc(L2-L1),
  clause_top_layer(Type,[pp(ICH)|Displaced],L1-L0).
clause_top_layer(Type,Displaced,[IML|L]-L) -->
  clause_top_layer(Type,Displaced,IMLL1-[]),
  clause_top_tail(Type,Displaced,L1-[]),
  {
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1]
  }.

clause_top_tail(Type,Displaced,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  clause_top_layer(Type,Displaced,IMLL-[]),
  {
    IML =.. ['IML'|IMLL]
  }.
clause_top_tail(Type,Displaced,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  clause_top_layer(Type,Displaced,IMLL-[]),
  clause_top_tail(Type,Displaced,L-L0),
  {
    IML =.. ['IML'|IMLL]
  }.

% Verb phrase layer

verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,L) -->
  verb_with_complement(Displaced,Sbj_type,Infl,Voice,L).
verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,L-L0) -->
  adverb_phrase(L-L1),
  verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,L1-L0).
verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,L-L0) -->
  verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,L-L1),
  adverbial(L1-L0).
verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,[IML|L]-L) -->
  verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,IMLL1-[]),
  verb_phrase_tail(Displaced,Sbj_type,Infl,Voice,L1-[]),
  {
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1]
  }.

verb_phrase_tail(Displaced,Sbj_type,Infl,Voice,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,IMLL-[]),
  {
    IML =.. ['IML'|IMLL]
  }.
verb_phrase_tail(Displaced,Sbj_type,Infl,Voice,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  verb_phrase_layer(Displaced,Sbj_type,Infl,Voice,IMLL-[]),
  verb_phrase_tail(Displaced,Sbj_type,Infl,Voice,L-L0),
  {
    IML =.. ['IML'|IMLL]
  }.

% Clause operator layer

operator_layer(L) -->
  operator_inside(L).
operator_layer(L-L0) -->
  operator_inside(L-L1),
  neg(L1-L0).

operator_inside(L) -->
  modal(L).
operator_inside(L) -->
  {
    member(Tag,['DOP','DOD'])
  },
  verb(Tag,'',L).

% HAVE, BE or modal catenative finite layer

have_be_or_mod_cat_finite_layer(Tag,Code,L) -->
  have_be_or_mod_cat_finite_inside(Tag,Code,L).
have_be_or_mod_cat_finite_layer(Tag,Code,L-L0) -->
  have_be_or_mod_cat_finite_inside(Tag,Code,L-L1),
  neg(L1-L0).

have_be_or_mod_cat_finite_inside(Tag,Code,L) -->
  {
    member(Tag,['HVP','HVD']),
    verb_code('H',Code)
  },
  verb(Tag,Code,L).
have_be_or_mod_cat_finite_inside(Tag,Code,L) -->
  {
    member(Tag,['BEP','BED']),
    verb_code('B',Code)
  },
  verb(Tag,Code,L).
have_be_or_mod_cat_finite_inside('MD',';~cat_Vt',L) -->
  modal_catenative(L).

% TO infinitive layer

ip_to_inf(Ext,Displaced,Sbj_type,[IP|L]-L) -->
  to_inf_layer(Displaced,Sbj_type,VPL-[]),
  {
    atom_concat('IP-INF',Ext,Label),
    IP =.. [Label|VPL]
  }.

to_inf_layer(Displaced,Sbj_type,L-L0) -->
  to(L-L1),
  verb_phrase_layer(Displaced,Sbj_type,infinitive,active,L1-L0).
to_inf_layer(Displaced,unfilled_subject,L-L0) -->
  conn(L-L2),
  noun_phrase('-SBJ',L2-L1),
  to_inf_layer(Displaced,filled_subject,L1-L0).
to_inf_layer(Displaced,Sbj_type,L-L0) -->
  adverb_phrase(L-L1),
  to_inf_layer(Displaced,Sbj_type,L1-L0).
to_inf_layer(Displaced,Sbj_type,[IML|L]-L) -->
  to_inf_layer(Displaced,Sbj_type,IMLL1-[]),
  to_inf_tail(Displaced,Sbj_type,L1-[]),
  {
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1]
  }.

to_inf_tail(Displaced,Sbj_type,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  to_inf_layer(Displaced,Sbj_type,IMLL-[]),
  {
    IML =.. ['IML'|IMLL]
  }.
to_inf_tail(Displaced,Sbj_type,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  to_inf_layer(Displaced,Sbj_type,IMLL-[]),
  to_inf_tail(Displaced,Sbj_type,L-L0),
  {
    IML =.. ['IML'|IMLL]
  }.

% Verb sequencing

ip_ppl(Ext,Displaced,Sbj_type,Infl,[IML|L]-L) -->
  verb_phrase_layer(Displaced,Sbj_type,Infl,active,VPL-[]),
  {
    atom_concat('IP-PPL',Ext,Label),
    IML =.. [Label|VPL]
  }.

% Verb sequencing with passive

ip_ppl_passive(Ext,Displaced,Sbj_type,[IML|L]-L) -->
  verb_phrase_layer(Displaced,Sbj_type,en_participle,passive,VPL-[]),
  {
    atom_concat('IP-PPL',Ext,Label),
    IML =.. [Label,'NP-LGS'('*')|VPL]
  }.

% Clause middle layer

clause_middle_layer(Displaced,Sbj_type,L-L0) -->
  operator_layer(L-L1),
  verb_phrase_layer(Displaced,Sbj_type,infinitive,active,L1-L0).
clause_middle_layer(Displaced,Sbj_type,L-L0) -->
  have_be_or_mod_cat_finite_layer(Tag,Code,L-L1),
  verb_complement(Tag,Code,Displaced,Sbj_type,active,L1-L0).
clause_middle_layer(Displaced,Sbj_type,L-L0) -->
  adverb_phrase(L-L1),
  clause_middle_layer(Displaced,Sbj_type,L1-L0).
clause_middle_layer(Displaced,Sbj_type,[IML|L]-L) -->
  clause_middle_layer(Displaced,Sbj_type,IMLL1-[]),
  clause_middle_tail(Displaced,Sbj_type,L1-[]),
  {
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1]
  }.

clause_middle_tail(Displaced,Sbj_type,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  clause_middle_layer(Displaced,Sbj_type,IMLL-[]),
  {
    IML =.. ['IML'|IMLL]
  }.
clause_middle_tail(Displaced,Sbj_type,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  clause_middle_layer(Displaced,Sbj_type,IMLL-[]),
  clause_middle_tail(Displaced,Sbj_type,L-L0),
  {
    IML =.. ['IML'|IMLL]
  }.

% CP-THT (that clause)

cp_that(Ext,Displaced,[CP|L]-L) -->
  clause_that_layer(Displaced,IPL-[]),
  {
    atom_concat('CP-THT',Ext,Label),
    IP =.. ['IP-SUB'|IPL],
    CP =.. [Label,IP]
  }.
cp_that(Ext,Displaced,[CP|L]-L) -->
  clause_that_layer(Displaced,IMLL1-[]),
  cp_that_tail(Displaced,L1-[]),
  {
    atom_concat('CP-THT',Ext,Label),
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1],
    CP =.. [Label,'IP-SUB'(IML)]
  }.

cp_that_tail(Displaced,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  clause_that_layer(Displaced,IPL-[]),
  {
    IML =.. ['IML'|IPL]
  }.
cp_that_tail(Displaced,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  clause_that_layer(Displaced,IPL-[]),
  cp_that_tail(Displaced,L-L0),
  {
    IML =.. ['IML'|IPL]
  }.

clause_that_layer([np(ICH)|Displaced],['NP-SBJ'(ICH)|L]-L0) -->
  verb_phrase_layer(Displaced,filled_subject,finite,active,L-L0).
clause_that_layer([np(ICH)|Displaced],['NP-SBJ'(ICH)|L]-L0) -->
  clause_middle_layer(Displaced,filled_subject,L-L0).
clause_that_layer(Displaced,L-L0) -->
  comp(L-L1),
  clause_top_layer(statement,Displaced,L1-L0).
clause_that_layer(Displaced,L) -->
  clause_top_layer(statement,Displaced,L).

% CP-QUE (embedded question clause)

cp_que(Ext,Displaced,[CP|L]-L) -->
  clause_que_finite_top_layer(Displaced,IPL-[]),
  {
    atom_concat('CP-QUE',Ext,Label),
    IP =.. ['IP-SUB'|IPL],
    CP =.. [Label,IP]
  }.
cp_que(Ext,Displaced,[CP|L]-L) -->
  clause_que_finite_top_layer(Displaced,IMLL1-[]),
  cp_que_finite_tail(Displaced,L1-[]),
  {
    atom_concat('CP-QUE',Ext,Label),
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1],
    CP =.. [Label,'IP-SUB'(IML)]
  }.
cp_que(Ext,Displaced,[CP|L]-L) -->
  clause_que_to_infinitive_top_layer(Displaced,IPL-[]),
  {
    atom_concat('CP-QUE',Ext,Label),
    IP =.. ['IP-INF'|IPL],
    CP =.. [Label,IP]
  }.
cp_que(Ext,Displaced,[CP|L]-L) -->
  clause_que_to_infinitive_top_layer(Displaced,IMLL1-[]),
  cp_que_to_infinitive_tail(Displaced,L1-[]),
  {
    atom_concat('CP-QUE',Ext,Label),
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1],
    CP =.. [Label,'IP-INF'(IML)]
  }.

cp_que_finite_tail(Displaced,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  clause_que_finite_top_layer(Displaced,IPL-[]),
  {
    IML =.. ['IML'|IPL]
  }.
cp_que_finite_tail(Displaced,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  clause_que_finite_top_layer(Displaced,IPL-[]),
  cp_que_finite_tail(Displaced,L-L0),
  {
    IML =.. ['IML'|IPL]
  }.

cp_que_to_infinitive_tail(Displaced,['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  clause_que_to_infinitive_top_layer(Displaced,IPL-[]),
  {
    IML =.. ['IML'|IPL]
  }.
cp_que_to_infinitive_tail(Displaced,[PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  clause_que_to_infinitive_top_layer(Displaced,IPL-[]),
  cp_que_to_infinitive_tail(Displaced,L-L0),
  {
    IML =.. ['IML'|IPL]
  }.

clause_que_finite_top_layer(Displaced,L-L0) -->
  comp_wq(L-L1),
  clause_top_layer(statement,Displaced,L1-L0).
clause_que_finite_top_layer(Displaced,L-L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,L-L1),
  clause_que_finite_lower_layer([np(ICH)|Displaced],L1-L0).
clause_que_finite_top_layer(Displaced,L-L0) -->
  adverb_phrase(L-L1),
  clause_top_layer(statement,Displaced,L1-L0).

clause_que_finite_lower_layer([np(ICH)|Displaced],['NP-SBJ'(ICH)|L]-L0) -->
  verb_phrase_layer(Displaced,filled_subject,finite,active,L-L0).
clause_que_finite_lower_layer([np(ICH)|Displaced],['NP-SBJ'(ICH)|L]-L0) -->
  clause_middle_layer(Displaced,filled_subject,L-L0).
clause_que_finite_lower_layer(Displaced,L) -->
  clause_top_layer(statement,Displaced,L).

clause_que_to_infinitive_top_layer(Displaced,L-L0) -->
  comp_wq(L-L1),
  to_inf_layer(Displaced,filled_subject,L1-L0).
clause_que_to_infinitive_top_layer(Displaced,L-L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  noun_phrase(Index,L-L1),
  to_inf_layer([np(ICH)|Displaced],filled_subject,L1-L0).
clause_que_to_infinitive_top_layer(Displaced,L-L0) -->
  adverb_phrase(L-L1),
  to_inf_layer(Displaced,filled_subject,L1-L0).

% notional subject

notional_subject(L) -->
  cp_that('-NSBJ',[],L).
notional_subject(L) -->
  cp_que('-NSBJ',[],L).
notional_subject(L) -->
  ip_to_inf('-NSBJ',[],_Sbj_type,L).
notional_subject(L) -->
  ip_ppl('-NSBJ',[],filled_subject,ing_participle,L).

% Adverbials

adverbial(L) -->
  adverb_phrase(L).
adverbial(L) -->
  scon_clause(L).

% subordinate conjunction clause

scon_clause([PP_SCON|L]-L) -->
  conn(PP_SCONL-[IP]),
  clause_top_layer(statement,[],IPL-[]),
  {
    IP =.. ['IP-ADV'|IPL],
    PP_SCON =.. ['PP-SCON'|PP_SCONL]
  }.
scon_clause([PP_SCON|L]-L) -->
  conn(PP_SCONL-L1),
  ip_to_inf('',[],filled_subject,L1-[]),
  {
    PP_SCON =.. ['PP-SCON'|PP_SCONL]
  }.
scon_clause([PP_SCON|L]-L) -->
  conn(PP_SCONL-L1),
  ip_ppl('',[],filled_subject,ing_participle,L1-[]),
  {
    PP_SCON =.. ['PP-SCON'|PP_SCONL]
  }.
scon_clause([PP_SCON|L]-L) -->
  scon_clause([PP_SCONL]-[]),
  scon_clause_tail(L1-[]),
  {
    PP_SCON =.. ['PP',PP_SCONL|L1]
  }.

scon_clause_tail(['CONJP'(CONJ,PP_SCON)|L]-L) -->
  conj(CONJ),
  scon_clause([PP_SCON]-[]).
scon_clause_tail([PU,'CONJP'(PP_SCON)|L]-L0) -->
  punc(non_final,[PU]-[]),
  scon_clause([PP_SCON]-[]),
  scon_clause_tail(L-L0).

% Relative clauses

relative_clause([IP|L]-L) -->
  relative_clause_top_layer(IPL-[]),
  {
    IP =.. ['IP-REL'|IPL]
  }.
relative_clause(L) -->
  ip_ppl('',[],filled_subject,ing_participle,L).
relative_clause(L) -->
  ip_ppl_passive('',[],filled_subject,L).
relative_clause(L) -->
  ip_to_inf('-REL',['np'('*T*')],_Sbj_type,L).

relative_clause_top_layer(L-L0) -->
  {
    gensym('-',Index),
    atom_concat('*ICH*',Index,ICH)
  },
  relative_noun_phrase(Index,L-L1),
  relative_clause_inside([np(ICH)],L1-L0).
relative_clause_top_layer(L) -->
  clause_top_layer(statement,[np('*T*')],L).
relative_clause_top_layer([IML|L]-L) -->
  relative_clause_top_layer(IMLL1-[]),
  relative_clause_top_tail(L1-[]),
  {
    IML1 =.. ['IML'|IMLL1],
    IML =.. ['IML',IML1|L1]
  }.

relative_clause_top_tail(['CONJP'(CONJ,IML)|L]-L) -->
  conj(CONJ),
  relative_clause_top_layer(IMLL-[]),
  {
    IML =.. ['IML'|IMLL]
  }.
relative_clause_top_tail([PU,'CONJP'(IML)|L]-L0) -->
  punc(non_final,[PU]-[]),
  relative_clause_top_layer(IMLL-[]),
  relative_clause_top_tail(L-L0),
  {
    IML =.. ['IML'|IMLL]
  }.

relative_noun_phrase(Ext,[NP|L]-L) -->
  relative_noun_head_full(NPL-[]),
  {
    atom_concat('NP',Ext,Label),
    NP =.. [Label|NPL]
  }.

relative_clause_inside([np(ICH)],['NP-SBJ'(ICH)|L]-L0) -->
  verb_phrase_layer([],filled_subject,finite,active,L-L0).
relative_clause_inside([np(ICH)],['NP-SBJ'(ICH)|L]-L0) -->
  clause_middle_layer([],filled_subject,L-L0).
relative_clause_inside(Displaced,L) -->
  clause_top_layer(statement,Displaced,L).

