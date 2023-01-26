// A shim for safari: https://developer.chrome.com/blog/using-requestidlecallback/

// Provides: requestIdleCallbackShim
function requestIdleCallbackShim (cb) {
    if (window.requestIdleCallback) {
        window.requestIdleCallback(cb)
    } else {
        var start = Date.now();
        globalThis.setTimeout(function () {
            cb({
                didTimeout: false,
                timeRemaining: function () {
                    return Math.max(0, 50 - (Date.now() - start));
                }
            });
        }, 1);
    }
}

// Provides: cancelIdleCallbackShim
function cancelIdleCallbackShim (id) {
    if (window.cancelIdleCallback) {
        window.cancelIdleCallback(id);
    } else {
        globalThis.clearTimeout(id);
    }
}