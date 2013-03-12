Property testing:

pridame radky s vlastnostmi, ktere to ma mit. Napr u funkce reverse:
> prop_length = length s = length (reverse s)
> prop_id = s == (reverse . reverse) s

dokumentacni 'standart' haddoc
..

cabal unpack balicek : rozbali zdroje balicku 

--------------------------------------------------------------------------------
quickCheck
--------------------------------------------------------------------------------

quickCheckWith (stdArgs {maxSucces = 10000}) fce_na_testovani

> quickCheckWith ( stdArgs {maxSuccess = 10000}) prop_id2

