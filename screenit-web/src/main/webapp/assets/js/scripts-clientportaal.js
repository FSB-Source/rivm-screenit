/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
function setBaseFontSize(zoom)
{
	var zoomFactor = localStorage.getItem('fontSize');

	if (zoom == 'plus')
	{
		if (!zoomFactor || zoomFactor == 'font-normal')
		{
			newZoomFactor = 'font-large';
			$('.min-icon').removeClass('font-disabled');
		}
		else if (zoomFactor == 'font-large')
		{
			newZoomFactor = 'font-xlarge';
			$('.plus-icon').addClass('font-disabled');
		}
	}
	else if (zoom == 'min')
	{
		if (zoomFactor == 'font-xlarge')
		{
			newZoomFactor = 'font-large';
		}
		else
		{
			newZoomFactor = 'font-normal';
			$('.min-icon').addClass('font-disabled');
		}
		$('.plus-icon').removeClass('font-disabled');
	}
	else
	{
		newZoomFactor = zoomFactor;
	}

	$('body').removeClass(function(index, css)
	{
		return (css.match(/\bfont-\S+/g) || []).join(' ');
	}).addClass(newZoomFactor);

	localStorage.setItem('fontSize', newZoomFactor);
}

function showSuccessFeedbackMsg()
{
	$alert = $('.alert-success');
	if ($alert.length > 0)
	{
		$('body').append('<div class="alert-overlay"></div>');
	}

	$alert.show(function()
	{
		$(this).addClass('fadeInDown');
		$('.close-alert', this).on('click', function()
		{
			if (!Modernizr.cssanimations)
			{
				$alert.fadeOut();
			}
			else
			{
				$alert.addClass('fadeOutUp');
			}

			$('body .alert-overlay').fadeOut(function()
			{
				$(this).remove();
			});
			setTimeout(function () {$alert.remove(); }, 1000);
		});
	});
}

function setWizardStep(item, state)
{
	if (state == 'done')
	{
		$('.wizard-nav .active').addClass('done');
	}

	$('.wizard-nav .active').removeClass('active');
	$('.' + item).addClass('active');
}
