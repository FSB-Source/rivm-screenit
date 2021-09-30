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
angular.module('rivmUistrijkendarts').directive('thisearlierthan', function()
{
	return {
		require: 'ngModel',
		restrict: 'A',
		link: function(scope, elem, attrs, ctrl)
		{
			var startDate, endDate;

			scope.$watch(attrs.ngModel, function(newVal, oldVal, scope)
			{
				endDate = newVal;
				check();
			});

			scope.$watch(attrs.thisearlierthan, function(newVal, oldVal, scope)
			{
				startDate = newVal;
				check();
			});

			var check = function()
			{
				if (typeof startDate === 'undefined' || startDate === null || typeof endDate === 'undefined' || endDate === null)
				{
					ctrl.$setValidity('thisearlierthan', true);
					return;
				}

				if (!validate(startDate))
				{
					startDate = new Date(startDate);
					if (!validate(startDate))
					{
						return;
					}
				}

				if (!validate(endDate))
				{
					endDate = new Date(endDate);
					if (!validate(endDate))
					{
						return;
					}
				}

				if (startDate <= endDate)
				{
					ctrl.$setValidity('thisearlierthan', true);
				}
				else
				{
					ctrl.$setValidity('thisearlierthan', false);
				}

				return;
			};

			var validate = function(date)
			{
				if (Object.prototype.toString.call(date) === '[object Date]')
				{
					if (isNaN(date.getTime()))
					{
						return false;
					}
					else
					{
						return true;
					}
				}
				else
				{
					return false;
				}
			};
		}
	};
});
