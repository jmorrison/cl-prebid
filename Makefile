.PHONY: test-prebid test-cl-prebid

Prebids.js/build/dev/prebid.js: Prebid.js
	. ${NVM_DIR}/nvm.sh ; nvm install v22  ; nvm use v22 ; pushd Prebid.js ; npm ci --loglevel=silly -d -dd -ddd ; ./node_modules/gulp/bin/gulp.js build --modules=openxBidAdapter,rubiconBidAdapter,sovrnBidAdapter ; node_modules/gulp/bin/gulp serve

#Prebids.js/build/dev/prebid.js: Prebid.js
#	( . ${NVM_DIR}/nvm.sh ; nvm install v22 )
#	( . ${NVM_DIR}/nvm.sh ; nvm use v22 )
#	( pushd Prebid.js && . ${NVM_DIR}/nvm.sh && npm ci ) # Does not quite work right

Prebid.js:
	git clone https://github.com/prebid/Prebid.js.git

test-prebid: Prebids.js/dev/prebid.js.map
	( pushd Prebid.js && ./node_modules/.bin/gulp serve )

test-cl-prebid: Prebids.js/build/dev/prebid.js
	sbcl --eval "(ql:quickload '(:weblocks :cxml :cl-prebid/hunchentoot))" --eval '(cl-prebid/hunchentoot::run)'
