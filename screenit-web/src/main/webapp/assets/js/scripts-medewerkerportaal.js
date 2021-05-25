/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
$(function()
{
	$(window).resize(function()
	{
		calCanvasHeight();
	});

	calCanvasHeight();

});

function calCanvasHeight()
{
	if ($('#jp-container').length <= 0)
	{
		return;
	}

	var topnavHeight = $('#jp-container').offset().top;
	var marginBottom = 0;

	if ($('body').hasClass('client'))
	{
		marginBottom = 20;
	}

	var footerHeight = $('.footer').height();
	var minWindowHeight = 250;

	var assetsCombinedHeight = topnavHeight + footerHeight + marginBottom;
	var canvasHeight = $(window).height();
	if (canvasHeight > minWindowHeight)
	{
		canvasHeight = canvasHeight - assetsCombinedHeight;
	}
	else
	{
		canvasHeight = minWindowHeight;
	}

	$('.canvas.content').css({
		'height': canvasHeight
	});

}

function initRoosterDatepicker(markupId)
{

	$("#" + markupId).addClass('text unit-100');
	$("#" + markupId).datepicker({
		onSelect: function(dateText, inst)
		{
			var date = $.datepicker.parseDate('dd-mm-yy', dateText);
			$('.calender-container > div').medewerkersKalender('gotoWeek', date);
		},
		buttonImageOnly: false,
		showOn: "focus",
		numberOfMonths: 3,
		dateFormat: 'dd-mm-yy',
		monthNames: ['Januari', 'Februari', 'Maart', 'April', 'Mei', 'Juni', 'Juli', 'Augustus', 'September', 'Oktober', 'November', 'December']
	});
}

function initTooltip()
{
	$('*[data-tooltip]').qtip({
		content: {
			text: function(api)
			{
				var tooltiptekst = $(this).attr('data-tooltip');
				if (!(/\s/g.test(tooltiptekst)))
				{
					var $tooltipHtml = $('.' + tooltiptekst).html();
					if ($tooltipHtml != null)
					{
						tooltiptekst = $tooltipHtml;
					}
				}
				return tooltiptekst;
			}
		},

		show: {
            solo: true
        },         
        hide: {
            fixed: true,
            when: { event: 'inactive' }, 
            delay: 1500
        },
		position: {
			viewport: true,
		},
		style: {
			classes: 'ui-tooltip-colonis'
		},		
	});
}

function initNullFlavourFields()
{
	$('.bevatNullFlavour :text').on("change keyup paste blur", function () {
		if ($.trim($(this).val()).length > 0) {
			$(this).parent().find(":checkbox").prop( "checked", false );
		}
	});

	$('.bevatNullFlavour :checkbox').change(function () {
		if (this.checked) {
			$(this).parent().find(":text").val("");
		}
	});

}

function toggleBkAfsprakenCheckboxes($blokCheckbox) {
	$blokCheckbox.closest('table').find('td input[type=checkbox]').prop('checked', $blokCheckbox.prop('checked') === true);
}

function toggleBkAfsprakenSuperCheckboxes($blokCheckbox) {
	if ($blokCheckbox.prop('checked') === false) {
		$blokCheckbox.closest('table').find('th input[type=checkbox]').prop('checked', false);
		$('.selectAllCheckbox').prop('checked', false);
	}
}
