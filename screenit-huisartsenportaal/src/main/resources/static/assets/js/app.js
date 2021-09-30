/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
'use strict';

var huisartsportaal = {};

angular.module('rivmUistrijkendarts', ['ngRoute',
    'ngResource',
    'ngAnimate',
    'ngTouch',
    'ngMessages',
    'ngSanitize',
    'ui.bootstrap',
    'ui.select',
    'toaster',
    'angular-oauth2',
    'http-auth-interceptor',
    'ngTable',
    'ngCsv']).config(config).run(run);

function config($routeProvider, $httpProvider, OAuthProvider, OAuthTokenProvider, $locationProvider) {
    $routeProvider.when('/', {
        templateUrl: 'assets/js/login/login.tmp.html',
        controller: 'loginCtrl',
        controllerAs: 'login'
    }).when('/browserverouderd', {
        templateUrl: 'assets/js/localstorage/localStorage.tmp.html',
        controller: 'storageCtrl',
        controllerAs: 'storage'
    }).when('/inner/', {
        templateUrl: 'assets/js/intern/labformulieren/labformulieren.tmp.html',
        controller: 'labformulierenCtrl',
        controllerAs: 'form'
    }).when('/inner/gegevens/', {
        templateUrl: 'assets/js/intern/wijziggegevens/wijziginggegevens.tmp.html',
        controller: 'wijzigenGegevensCtrl',
        controllerAs: 'reg'
    }).when('/inner/nieuweovereenkomst/', {
        templateUrl: 'assets/js/intern/wijziggegevens/wijziginggegevens.tmp.html',
        controller: 'wijzigenGegevensCtrl',
        controllerAs: 'reg'
    }).when('/wachtwoordvergeten/voltooien/', {
        templateUrl: 'assets/js/login/wachtwoordVergetenVoltooien.tmp.html',
        controller: 'wachtwoordVergetenVoltooienCtrl',
        controllerAs: 'wachtwoord'
    }).when('/wachtwoordvergeten/registreren/:code', {
        templateUrl: 'assets/js/login/wachtwoordVergetenRegistreren.tmp.html',
        controller: 'wachtwoordVergetenRegistrerenCtrl',
        controllerAs: 'wachtwoord'
    }).when('/wachtwoordvergeten/registreren/', {
        templateUrl: 'assets/js/login/wachtwoordVergetenRegistreren.tmp.html',
        controller: 'wachtwoordVergetenRegistrerenCtrl',
        controllerAs: 'wachtwoord'
    }).when('/wachtwoordvergeten', {
        templateUrl: 'assets/js/login/wachtwoordVergeten.tmp.html',
        controller: 'wachtwoordCtrl',
        controllerAs: 'wachtwoord'
    }).when('/registreren/voltooien/', {
        templateUrl: 'assets/js/registratie/registratieVoltooien.tmp.html',
        controller: 'registrerenVoltooienCtrl',
        controllerAs: 'reg'
    }).when('/registreren/controle/', {
        templateUrl: 'assets/js/registratie/registratieControle.tmp.html',
        controller: 'registrerenControleCtrl',
        controllerAs: 'reg'
    }).when('/registreren/:agbcode/', {
        templateUrl: 'assets/js/registratie/registratieInlogcode.tmp.html',
        controller: 'registrerenInlogcodeCtrl',
        controllerAs: 'reg'
    }).when('/registreren', {
        templateUrl: 'assets/js/registratie/registratieInlogcode.tmp.html',
        controller: 'registrerenInlogcodeCtrl',
        controllerAs: 'reg'
    }).when('/inner/verrichtingen', {
        templateUrl: 'assets/js/intern/verrichtingen/verrichtingenOverzicht.tmp.html',
        controller: 'verrichtingenCtrl',
        controllerAs: 'form'
    }).when('/inner/betalingen', {
        templateUrl: 'assets/js/intern/betalingen/betalingenOverzicht.tmp.html',
        controller: 'betalingenCtrl',
        controllerAs: 'form'
    }).when('/verifieren', {
        templateUrl: 'assets/js/verifieren/klantnummerVerificatie.tmp.html',
        controller: 'verifierenCtrl',
        controllerAs: 'ctrl'
    }).otherwise({
        redirectTo: '/'
    });

    $httpProvider.interceptors.push('authHttpResponseInterceptor');
};

function run($rootScope, $interval, userfactory, OAuth, $location, toaster) {
    var userTimeout, accessTokenCall

    document.onclick = function() {
        resetUserTimeout();
    };

    $interval(checkUserActivity, 10000);

    function resetUserTimeout() {
        var nexttime = new Date();
        nexttime.setMinutes(nexttime.getMinutes() + 30);
        userTimeout = nexttime.getTime();
    }

    function checkUserActivity() {
        if (OAuth.isAuthenticated()) {
            if (userTimeout < Date.now()) {
                userfactory.logout();
            }
        }
    }

    $interval(checkUserActivity, 10000);

    $rootScope.$on('event:auth-loginRequired', function(event, rejection) {
        if (rejection.data) {

            if (rejection.data.error === 'invalid_grant') {
                return;
            }

            if (rejection.data.error === 'invalid_token' && (!accessTokenCall || accessTokenCall.$$state.status > 0)) {
                accessTokenCall = OAuth.getRefreshToken();
                accessTokenCall.then(function(response) {
                    authService.loginConfirmed('success', function(config) {
                        config.headers.Authorization = OAuthToken.getAuthorizationHeader();
                        return config;
                    });
                    return response;
                }, function() {
                    userfactory.logOut();
                });
                return accessTokenCall;
            }

            if (rejection.data.error_description.indexOf('refresh token') > -1) {
                userfactory.logout();
            }

        }
    });

    $rootScope.$on('$locationChangeSuccess', function() {
        $rootScope.actualLocation = $location.path();
    });

    $rootScope.$watch(function() {
        return $location.path()
    }, function(newLocation, oldLocation) {
        if ($rootScope.actualLocation === newLocation && oldLocation === '/browserverouderd') {
            $location.path(oldLocation);
        }
    });

    var hasStorage = (function() {
        var mod = "test";
        try {
            localStorage.setItem(mod, mod);
            localStorage.removeItem(mod);
            return true;
        }
        catch (exception) {
            return false;
        }
    }());

    if (!hasStorage) {
        $location.path('/browserverouderd');
    }
}
