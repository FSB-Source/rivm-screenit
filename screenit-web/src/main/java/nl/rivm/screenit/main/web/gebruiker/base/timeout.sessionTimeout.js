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
(function (screenit, $, undefined) {
	screenit.meldingTimeout = 10000;
	screenit.keepAliveCallback = null;
	screenit.logoutCallback = null;
	screenit.isPolling = false;

	const Notification = window.Notification || window.mozNotification || window.webkitNotification;

	if (Notification) {
		Notification.requestPermission(function (permission) {
			console.log(permission);
		});
	}

	let interval;
	let tijdGestart = null;
	let checkTijdInterval;

	function sessieBijnaVerlopen() {
		$("#sessieVerlopenDialog").modal({
			show: true,
			backdrop: 'static'
		});
		countdown();

		if (Notification && Notification.permission !== "denied") {
			window.setTimeout(function () {
				const instance = new Notification("ScreenIT - Timeout waarschuwing", {
					body: "Uw sessie verloopt binnen 5 minuten.",
					icon: "../assets/favicon/favicon-160x160.png"
				});
				instance.onclick = function () {
					this.close();
				};
			}, 5000);
		}
	}

	screenit.startSessionTimer = function () {
		tijdGestart = new Date().getTime();
		checkTijdInterval = setInterval(function () {
			const huidigeTijd = new Date().getTime();
			const tijdVerschil = huidigeTijd - tijdGestart;

			if (tijdVerschil > screenit.meldingTimeout) {
				sessieBijnaVerlopen();
				clearInterval(checkTijdInterval);
			}
		}, 1000 * 9);
	};

	screenit.resetTimer = function () {
		clearInterval(interval);
		document.title = "ScreenIT";
	};

	screenit.setIsPolling = function (isPolling) {
		screenit.isPolling = isPolling;
	};

	$(document).ajaxSend(function (event, request, settings) {
		if (!screenit.isPolling || settings.url.indexOf("triggeredByPollingAbstractAjaxTimerBehavior") == -1) {
			tijdGestart = new Date().getTime();
		}
	});

	function countdown(element) {
		let minutes = 5;
		let seconds = 0;

		interval = setInterval(function () {
			const el = document.getElementById("timer");
			if (seconds === 0) {
				if (minutes === 0) {
					clearInterval(interval);
					resetFormValues();
					screenit.logoutCallback();
					bevatFormulieren = false;
					window.location = "/";
					return;
				} else {
					minutes--;
					seconds = 60;
				}
			}
			const minute_text = minutes > 0 ? minutes + (minutes > 1 ? ' minuten' : ' minuut') : '';
			const second_text = seconds > 1 ? 'seconden' : 'seconde';
			el.innerHTML = minute_text + ' ' + seconds + ' ' + second_text;
			document.title = 'ScreenIT - ' + minutes + 'm' + seconds + 's (Uw sessie verloopt)';
			seconds--;
		}, 1000);
	}
}(window.screenit = window.screenit || {}, jQuery));

function clickCallBack() {
	$('.js-keepalive-call-back').on('click', function (event) {
		screenit.keepAliveCallback();
		screenit.startSessionTimer();
		screenit.resetTimer();
		$('#sessieVerlopenDialog').modal('hide')
	});
}

document.addEventListener('DOMContentLoaded', function () {
	clickCallBack();
});
