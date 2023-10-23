cabal build;

timeout 3600 cabal run mev-analysis -- "bw" "R" 2>&1 | tee eval/bw_R.log;
timeout 3600 cabal run mev-analysis -- "bw" "S" 2>&1 | tee eval/bw_S.log;

timeout 3600 cabal run mev-analysis -- "cp" "R" 2>&1 | tee eval/cp_R.log;
timeout 3600 cabal run mev-analysis -- "cp" "S" 2>&1 | tee eval/cp_S.log;

timeout 3600 cabal run mev-analysis -- "hl" "R" 2>&1 | tee eval/hl_R.log;
timeout 3600 cabal run mev-analysis -- "hl" "S" 2>&1 | tee eval/hl_S.log;

timeout 3600 cabal run mev-analysis -- "amm" "R" 2>&1 | tee eval/amm_R.log;
timeout 3600 cabal run mev-analysis -- "amm" "S" 2>&1 | tee eval/amm_S.log;

echo "done"
