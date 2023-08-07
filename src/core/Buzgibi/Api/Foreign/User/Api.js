import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const mkUserApi = function(api) {
    return () => {
        return new e.UserApi(api);
    }
}

export const _makeSurvey = function(withError, survey, api) {
    return function(onError, onOk) {
        api.userSurveyPut(survey).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _submitSurvey = function(withError, submit, api) {
    return function(onError, onOk) {
        api.userSurveySubmitPost(submit).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _getHistory = function(withError, page, api) {
    return function(onError, onOk) {
        api.userSurveyHistoryGet(page).then(onOk).catch(resp => {
            console.log(resp);
            return withError(resp, onError)
        })
    };
}