rm -rf .cpcache
rm -rf .shadow-cljs
rm -rf dist/**

yarn install --silent \
    && mkdir -p dist \
    && rm -rf dist/** \
    && clojure -M:dev:build

cp resources/public/index.html dist/index.html

