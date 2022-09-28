Code for the book "Exploring Layers and Ripples: Studying English with Computer Parsing"
by Alastair Butler (Hirosaki University)
https://ajb129.github.io/layers_and_ripples.html

To run the program you need a working version of the XSB Tabling Prolog system (http://xsb.sourceforge.net/).

Example session
===============

$ xsb

| ?- [main].

| ?- tphrase_set_string(['NS'('boys')]), parse(noun).

| ?- tphrase_set_string(['NS'('boys')]), parse(noun_phrase('-SBJ')).

| ?- tphrase_set_string(['NS'('boys')]), parse(utterance('ex1')).

| ?- tphrase_set_string(['VB;~Ipr'('look'),'P-ROLE'('at'),'D;_nphd_'('this')]), parse(verb_phrase_layer([],filled_subject,imperative,active)).

| ?- tphrase_set_string(['N'('Cheese'),'N'('pizza'),'VBD;~I'('smells'),'PU'('.')]), parse(utterance(ex2)).

| ?- tphrase_set_string(['N'('Cheese'),'N'('pizza'),'VBD;~Tn'('needs'),'PU'('.')]), parse(utterance(ex3)).

| ?- tphrase_set_string(['D'('the'),'N'('ascent'),'P-ROLE'('of'),'D'('a'),'ADJ'('human'),'N'('figure'),'P-ROLE'('from'),'D'('the'),'N'('direction'),'P-ROLE'('of'),'D'('the'),'ADJ'('distant'),'N'('town')]), parse(noun_phrase('')).

Licence
=======

All sources (c) 2022 Alastair Butler

This work is licensed under the Creative Commons Attribution
4.0 International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/ or send a letter to
Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

