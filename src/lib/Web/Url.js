export const _getQueryParam =
    function(nothing, just, s) {
        let currentUrl = window.location.href;
        let xs = currentUrl.slice(url.indexOf('?') + 1).split('&');
        let params = {};
        xs.map(param => {
            const [key, val] = param.split('=');
            params[key] = decodeURIComponent(val);
        });
        return params[s] === undefined ? nothing : just(params[s]);
    }