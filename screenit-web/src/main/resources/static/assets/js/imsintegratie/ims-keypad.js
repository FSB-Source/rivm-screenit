/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
var bsnVoorImsCheck; 

const supportedImsActions = [
    {
        contextType: 'Result',
        contextValue: 'OK',
    },
    {
        contextType: 'keyPressed',
        contextValue: 'CloseDialog',
    },
];

function openWebsocketToImsBridge(username) {
    var webSocket = new WebSocket('wss://localhost:7002');

    webSocket.onopen = function() {
        console.log('IMS: WebSocket opened');
    };

    webSocket.onmessage = function(event) {
        console.log('IMS: WebSocket message received ');
        verwerkWebsocketBericht(event, username);
    };

    webSocket.onerror = function() {
        imsErrorCallback('websocketError');
    };

    webSocket.onclose = function(event) {
        console.log('IMS: WebSocket is closed with code: ' + event.code);
    };
}

function verwerkWebsocketBericht(event, expectedUsername) {
    var userSession = JSON.parse(event.data);
    var contextType = userSession.context.type;
    var contextValue = userSession.context.value;
    var user = userSession.user.identifier.value;
    var focusBsn = userSession.focus.patient.identifier.value;
    if (userSession.context.result !== undefined) {
        var contextBsn = userSession.context.result.patient.identifier.value;
    }

    if (user !== expectedUsername) {
        console.error('IMS: Keypad message for incorrect username');
        imsErrorCallback('username-incorrect');
        return;
    }

    if (contextBsn !== undefined) {
        if (focusBsn !== bsnVoorImsCheck || contextBsn !== bsnVoorImsCheck) {
            console.error('IMS: Keypad message for incorrect person');
            imsErrorCallback('outofsync');

            return;
        }
    }
    var action = {
        contextType: contextType,
        contextValue: contextValue,
    };
    if (!isSupportedAction(action)) {
        console.error('IMS: Unsupported keypad message: Context type = ' + contextType + ' , Context value = ' + contextValue);
        return;
    }
    verwerkButtonPressedAction(action);
}

function isSupportedAction(action) {
    for (var i = 0; i < supportedImsActions.length; i++) {
        if (supportedImsActions[i].contextValue === action.contextValue && supportedImsActions[i].contextType === action.contextType) {
            return true;
        }
    }
    return false;
}

function verwerkButtonPressedAction(action) {
    console.log('Button: ' + action.contextType + ' - ' + action.contextValue);
    if (action.contextType === 'Result' && action.contextValue === 'OK') {
        if (!$('.modal-backdrop').is(':visible')) {
            $('[x-keypad-bevestigen]')[0].click();
        }
    } else if (action.contextType === 'keyPressed' && action.contextValue === 'CloseDialog') {
        var gezienButton = $('.allImagesSeenButton');
        if (gezienButton.is(':visible')) {
            gezienButton[0].click();
        }
    }
}

function logOnAfrondenClick() 
{
    console.time('IMS: Afronden click till start sending desktopsync');
    console.log('IMS: \'Afronden\' clicked');
}
