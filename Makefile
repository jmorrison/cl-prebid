#
# Building and testing Common Lisp implementation of Prebid
#

#
# JavaScript dependencies are excruciatingly brittle.
#

NVM_VERSION=v22
#NPM_OPTIONS=--loglevel=silly -d -dd -ddd
#NPM_OPTIONS=-d -dd -ddd
#NPM_OPTIONS=-d 
NPM_OPTIONS=
#GULP_COMMAND=serve
GULP_COMMAND=serve-prod

#
# Build prebid.js.
#
# Cannot have separate rule for git checkout on account of npm
# changing the timestamp of the git directory on every run, which
# makes it older than the prebid.js JavaScript file proper.
#

PREBID_TAG=10.4.0
PREBID_JS=Prebid.js/build/dev/prebid.js
PREBID_MODULES=--modules=openxBidAdapter,rubiconBidAdapter,sovrnBidAdapter

$(PREBID_JS): 
	git clone --branch $(PREBID_TAG) https://github.com/prebid/Prebid.js.git
	. ${NVM_DIR}/nvm.sh ; nvm install $(NVM_VERSION) ; nvm use $(NVM_VERSION) ; pushd Prebid.js ; npm ci $(NPM_OPTIONS) ; ./node_modules/gulp/bin/gulp.js build $(PREBID_MODULES)

#$(PREBID_JS): Prebid.js
#	( . ${NVM_DIR}/nvm.sh ; nvm install $(NVM_VERSION) )
#	( . ${NVM_DIR}/nvm.sh ; nvm use $(NVM_VERSION) )
#	( pushd Prebid.js && . ${NVM_DIR}/nvm.sh && npm ci ) # Does not quite work right

#
# Testing targets
#

.PHONY: test-prebid test-cl-prebid test-cl-prebid2 test-reblocks

test-prebid: $(PREBID_JS)
	( pushd Prebid.js && ./node_modules/.bin/gulp serve $(PREBID_MODULES) )

#
# Here are the various Common Lisp web server prebid tests
#

test-cl-prebid: $(PREBID_JS)
	sbcl --eval "(ql:quickload '(:weblocks :cxml :cl-prebid/hunchentoot))" --eval '(cl-prebid/hunchentoot::run)'

test-cl-prebid2: $(PREBID_JS)
	sbcl --eval "(ql:quickload '(:weblocks :cxml :cl-prebid/hunchentoot))" --eval '(cl-prebid/weblocks::run)'

#
# Don't think I'll worry much about this right now
#

test-reblocks:
	sbcl --eval "(ql:quickload '(:cl-prebid/reblocks))" --eval "(reblocks-examples/simple-form::start)"
