/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2016 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

angular.module('rivmUistrijkendarts').directive('menuitem', function($rootScope, $location, userfactory)
{
	return {
		restrict: 'E',
		controller: 'menuitemCtrl',
		controllerAs: 'item',
		scope: {
			link: '@link',
			recht: '@recht'
		},
		bindToController: true,
		replace: true,
		transclude: true,
		template: '<li><a href="{{item.link}}" ng-transclude></a></li>',
		link: function($rootScope, $element, $attr)
		{

			$rootScope.$on('userChange', function()
			{
				checkRechten();
			});
			checkRechten();

			function checkRechten()
			{

				if (userfactory.heeftRecht($attr.recht) || userfactory.isIngelogd() == $attr.restricted)
				{
					$element.css('display', 'block');
				}
				else
				{
					$element.css('display', 'none');
				}
			}
		}
	}
}).controller('menuitemCtrl', function($rootScope, userfactory)
{

});
