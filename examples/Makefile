SOLC=solc 

build: \
	bin/Allower.bin-runtime\
	bin/AMM.bin-runtime\
	bin/Bank.bin-runtime\
	bin/Blacklist.bin-runtime\
	bin/BoundedWithdrawer.bin-runtime\
	bin/CoinPusher.bin-runtime\
	bin/Crowdfund.bin-runtime\
	bin/Echo.bin-runtime\
	bin/FakeAMM.bin-runtime\
	bin/GetSet.bin-runtime\
	bin/HashLock.bin-runtime\
	bin/IERC20.bin-runtime\
	bin/Password.bin-runtime\
	bin/Summer.bin-runtime\
	bin/TokenStore.bin-runtime\
	bin/TokenWithdrawer.bin-runtime\
	bin/Whitelist.bin-runtime\
	bin/Withdrawer.bin-runtime\
	

bin/%.bin-runtime: src/%.sol
	$(SOLC) --bin-runtime $< -o bin --overwrite
