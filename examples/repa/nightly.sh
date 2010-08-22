
bin/repabot \
	--scratch-dir /Users/benl/devel/buildbox/buildbox-head-devel/build \
	--with-ghc-snapshot /Users/benl/devel/ghc/snap/ghc-head-20100820-hacked.tgz \
	--nightly \
	--iterations 5 \
	--write    build/current \
	--against  build/baseline \
	--mailfrom benl@ouroborus.net \
	--mailto   benl@ouroborus.net
