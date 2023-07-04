import * as e from '../TTHouse.Api.Foreign.Scaffold/Scaffold/src/index';

export const mkFrontApi = function(api) {
    return () => {
        return new e.FrontApi(api);
    }
}

export const _init =
    function(withError, api) {
        return function(onError, onOk) {
            api.apiFrontendInitGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _showInit =
    function(obj) {
        return JSON.stringify(obj);
    }

export const _showTranslation =
    function(obj) {
        return JSON.stringify(obj);
    }

export const getShaCommit = (obj) => {
    return obj.getShaCommit();
}

export const _getLogLevel = obj => {
    return obj.getEnv().getLogLevel();
}

export const getShaCSSCommit = (obj) => {
    return obj.getShaCommitCss();
}

export const _getIsCaptcha = nothing => just => obj => {
    let env = obj.getEnv();
    return env !== undefined ? just(env.getIsCaptcha()) : nothing;
}

export const _getToTelegram = nothing => just => obj => {
    let env = obj.getEnv();
    return env !== undefined ? just(env.getToTelegram()) : nothing;
}

export const _loadTranslation =
    function(withError, lang, api) {
        return function(onError, onOk) {
            api.apiFrontendTranslateLangGet(lang).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const mkLogReq = function(build, payload) {
    let req = new e.FrontendLogRequest();
    req.setBuild(build);
    req.setPayload(payload);
    return () => {
        return req;
    };
}

export const _sendLog = function(withError, req, api) {
    return function(onError, onOk) {
        api.apiFrontendLogPut(req).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _showCookie = cookie => {
    return cookie.getName()
}

export const _getCookies = function(withError, api) {
    return function(onError, onOk) {
        api.apiFrontendCookiesGet().then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _getMeta = function(withError, page, api) {
    return function(onError, onOk) {
        api.apiFrontendMetaGet(page).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const getMetaDescription = meta => {
    return meta.getDescription();
}

export const _getKeyText = obj => {
    return obj.getKey();
}
export const _getValText = obj => {
    return obj.getValue();
}

export const getCookiesInit = (obj) => {
    return obj.getCookies();
}

export const _getTranslationMenu = obj => {
    return obj.getMenu();
}

export const _getTranslationPage = obj => {
    return obj.getPage();
}

export const _showMapMenuText = menu => {
    return "{ key: " + menu.getKey() + ", value: " + menu.getValue() + " }";
}

export const getTranslationCopyright = obj => {
    return obj.getCopyright();
}

export const _getTranslationMessenger = obj => {
    return obj.getMessenger();
}