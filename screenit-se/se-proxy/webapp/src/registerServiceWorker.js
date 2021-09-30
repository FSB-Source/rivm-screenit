/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

const isLocalhost = Boolean(
    window.location.hostname === 'localhost' ||

    window.location.hostname === '[::1]' ||

    window.location.hostname.match(
        /^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/,
    ),
);

export default function register() {
    if (process.env.NODE_ENV === 'production' && 'serviceWorker' in navigator) {

        const publicUrl = new URL(process.env.PUBLIC_URL, window.location);
        if (publicUrl.origin !== window.location.origin) {

            return;
        }

        window.addEventListener('load', () => {
            const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`;

            if (!isLocalhost) {

                registerValidSW(swUrl);
            } else {

                checkValidServiceWorker(swUrl);
            }
        });
    }
}

function registerValidSW(swUrl) {
    navigator.serviceWorker.register(swUrl).then(registration => {
        registration.onupdatefound = () => {
            const installingWorker = registration.installing;
            installingWorker.onstatechange = () => {
                if (installingWorker.state === 'installed') {
                    if (navigator.serviceWorker.controller) {

                        console.log('New content is available; please refresh.');
                    } else {

                        console.log('Content is cached for offline use.');
                    }
                }
            };
        };
    }).catch(error => {
        console.error('Error during service worker registration: ' + error);
    });
}

function checkValidServiceWorker(swUrl) {

    fetch(swUrl).then(response => {

        if (
            response.status === 404 ||
            response.headers.get('content-type').indexOf('javascript') === -1
        ) {

            navigator.serviceWorker.ready.then(registration => {
                registration.unregister().then(() => {
                    window.location.reload();
                });
            });
        } else {

            registerValidSW(swUrl);
        }
    }).catch(() => {
        console.log(
            'No internet connection found. AppView is running in offline mode.',
        );
    });
}

export function unregister() {
    if ('serviceWorker' in navigator) {
        navigator.serviceWorker.ready.then(registration => {
            registration.unregister();
        });
    }
}
