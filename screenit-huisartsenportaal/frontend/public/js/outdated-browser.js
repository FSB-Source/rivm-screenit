/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
outdatedBrowserRework({
	browserSupport: {
		'Chrome Headless': 57,
	},
	messages: {
		nl: {
			outOfDate: 'Helaas, het lukt niet om in te loggen op het huisartsenportaal',
			unsupported: 'Helaas, het lukt niet om in te loggen op het huisartsenportaal',
			update: {
				web: 'Dat komt omdat u een oudere webbrowser gebruikt. Een webbrowser is het programma dat u gebruikt voor internet.' +
					'<br>' +
					'<br>' +
					'Wilt u inloggen? Dan moet u een andere browser gebruiken. U kunt bijvoorbeeld Google Chrome, Mozilla Firefox of Microsoft Edge gebruiken.' +
					'<br>',
				googlePlay: 'Dat komt omdat u een oudere webbrowser gebruikt. Een webbrowser is het programma dat u gebruikt voor internet.' +
					'<br>' +
					'<br>' +
					'Wilt u inloggen? Dan moet u een andere browser gebruiken. U kunt bijvoorbeeld Google Chrome, Mozilla Firefox of Microsoft Edge gebruiken.' +
					'<br>',
				appStore: 'Dat komt omdat u een oudere webbrowser gebruikt. Een webbrowser is het programma dat u gebruikt voor internet.' +
					'<br>' +
					'<br>' +
					'Wilt u inloggen? Dan moet u een andere browser gebruiken. U kunt bijvoorbeeld Google Chrome, Mozilla Firefox of Microsoft Edge gebruiken.' +
					'<br>',
			},
			url: 'https://veiliginternetten.nl/themes/situatie/moet-ik-mijn-browser-updaten/',
		},
	},
});
