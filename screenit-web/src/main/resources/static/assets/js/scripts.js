/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
function changeInActief(id) {
	var $button = $("#" + id);
	$button.parent().find("a").removeClass("active");
	$button.addClass("active");
}

function showDialog(markupId) {
	var $dialog = $('#' + markupId);

	if ($dialog.hasClass('moveModalDialog')) {
		var $modals = $('body>.modal#' + markupId);
		if ($modals.length > 1) {
			$modals.remove();
		}
		while ($dialog.hasClass('moveModalDialog')) {
			$dialog.removeClass('moveModalDialog');
		}
		$dialog.appendTo($('body', document));
	}

	$backdrop = $('body>.modal-backdrop');
	if ($backdrop.length >= 1) {
		$backdrop.remove();
	}

	$dialog.modal('show');
	return $dialog;
}

var oldFormValues;

var copyOldFormValues;
$(function () {
	resetFormValues();
	$("[hidden]").hide();
});

function resetFormValues() {
	if (bevatFormulieren) {
		oldFormValues = getFormValues();
	}
	if (window.huidigeLezingGewijzigd) {
		window.updateHuidigeLezingInitieeleValues();
	}
}

function backupOldFormValues() {
	if (bevatFormulieren) {
		copyOldFormValues = oldFormValues;
	}
	if (window.huidigeLezingGewijzigd) {
		window.backupHuidigeLezingInitieeleValues();
	}
}

function zetBackupOldFormValuesTerug() {
	if (bevatFormulieren) {
		oldFormValues = copyOldFormValues;
	}
	if (window.huidigeLezingGewijzigd) {
		window.zetBackupHuidigeLezingInitieeleValuesTerug();
	}
}

var bevatFormulieren;

function checkForChangesBeforeClick(buttonMarkupId) {
	if (formChanged()) {
		var result = confirm('Site verlaten?\nWijzigingen die je hebt aangebracht, worden mogelijk niet opgeslagen.');
		if (result) {
			triggerChangesConfirmed(buttonMarkupId);
		}
	} else {
		triggerChangesConfirmed(buttonMarkupId);
	}
}

function triggerChangesConfirmed(buttonMarkupId) {
	$("#" + buttonMarkupId + "").trigger('changesConfirmedEvent');
}

function formChanged() {
	return (bevatFormulieren && oldFormValues !== getFormValues()) || (window.huidigeLezingGewijzigd && window.huidigeLezingGewijzigd());
}

window.onbeforeunload = function () {
	if (formChanged()) {
		setTimeout(function () {
			$(".wicket-ajax-indicator").hide();
		}, 2000);
		return 'Er zijn wijzigingen aangebracht die niet zijn opgeslagen.';
	}
};

function confirmUnsavedChanges() 
{
	if (formChanged()) {
		if (confirm('Er zijn wijzigingen aangebracht die niet zijn opgeslagen. Doorgaan met wisselen organisatie?')) {
			oldFormValues = getFormValues();
			return true;
		}
		return false;
	}
	return true;
}

function getFormValues() {
	var json = {};
	$('form')
		.each(function (i) {
			if ($(this).hasClass('not-tracked-for-changes-on-page') == false) {
				var heading = "form-" + i;
				var attrType = "";
				if (json[heading] == undefined) {
					json[heading] = {};
				}
				$(this)
					.find('input[type=text]:not(.select2-input,.select2-focusser),input[type=number]:not(.select2-input,.select2-focusser), input[type=radio]:checked, input[type=checkbox]:checked, textarea, select, form')
					.each(function (x) {

						if ($(this).hasClass('ui-autocomplete-input') == false && $(this).hasClass('not-tracked-for-changes-on-page') == false) {
							if ($(this).attr('type') !== undefined) {
								attrType = " - " + $(this).attr("type");
							}
							var elementId = this.tagName + attrType + "-" + x;
							json[heading][this.tagName + "-" + x] = $(this).val();
						}
					});
			}
		});
	formValues = JSON.stringify(json);
	return formValues;
}

function syncColumnHeights(source, destination) {
	var sourceHeight = $(source).height();
	var destinationHeight = $(destination).height();

	if (sourceHeight > destinationHeight) {
		$(destination).css({
			'height': sourceHeight
		});
	} else {
		$(source).css({
			'height': destinationHeight
		});
	}

}

function toggleChevron() {
	var $accordionToggle = $('.accordion-toggle');
	var $accordion = $accordionToggle.closest('.accordion');
	var direction = $accordion.attr('data-chevron-direction');

	var $ChevronCollapsed = (direction === 'left') ? 'icon-chevron-left' : 'icon-chevron-right';
	var $ChevronExpanded = 'icon-chevron-down';

	$accordionToggle.on('click', function (e) {
		var $chevron = $(this).closest('.accordion-group').find('i.icon-purple');
		var $chevrons = $accordion.find('i.icon-purple').not($chevron);
		$chevrons.removeClass($ChevronExpanded).addClass($ChevronCollapsed);

		if ($chevron.hasClass($ChevronCollapsed)) {
			$chevron.removeClass($ChevronCollapsed);
			$chevron.addClass($ChevronExpanded);
		} else {
			$chevron.removeClass($ChevronExpanded);
			$chevron.addClass($ChevronCollapsed);
		}
	});
}

function tNummerUpdate(serieSelector) {
	$(serieSelector).each(function () {
		if ($(this).val().length > 0 && $(this).is(':enabled')) {
			$(serieSelector).val($(this).val());
		}
		return false;
	});
}

function vragenLijstToggle() {
	$('.icon-collapse').click(function () {
		if ($('.icon-collapse img').attr('src') == '../assets/images/icons/icon-min.png') {
			$('.verslagbeoordeling').hide(300);
			$('.icon-collapse img').attr("src", "../assets/images/icons/icon-plus.png");
		} else {
			$('.verslagbeoordeling').show(300);
			$('.icon-collapse img').attr("src", "../assets/images/icons/icon-min.png");
		}
	});
}

var matched, browser;

jQuery.uaMatch = function (ua) {
	ua = ua.toLowerCase();

	var match = /(chrome)[ \/]([\w.]+)/.exec(ua) || /(webkit)[ \/]([\w.]+)/.exec(ua) || /(opera)(?:.*version|)[ \/]([\w.]+)/.exec(ua) || /(msie) ([\w.]+)/.exec(ua) || ua.indexOf("compatible") < 0 && /(mozilla)(?:.*? rv:([\w.]+)|)/.exec(ua) || [];

	return {
		browser: match[1] || "", version: match[2] || "0"
	};
};

matched = jQuery.uaMatch(navigator.userAgent);
browser = {};

if (matched.browser) {
	browser[matched.browser] = true;
	browser.version = matched.version;
}

if (browser.chrome) {
	browser.webkit = true;
} else if (browser.webkit) {
	browser.safari = true;
}

jQuery.browser = browser;

function cervixUAFindOutAdressesEquals() {
	if (isGelijk('straat') && isGelijk('huisnummer') && isGelijk('huisnummerToevoeging') && isGelijk('postcode') && isGelijk('plaats')) {
		$('.adressenGelijk').click();
	}
}

function isGelijk(fieldClass) {
	return $('input[type=text].postadres.' + fieldClass).val() == $('input[type=text].praktijkadres.' + fieldClass).val();
}

var isVisible = false;

var hideAllPopovers = function () {
	$('.info-icon').each(function () {
		$(this).popover('hide');
	});
};

function initInfoPopover(selector) {
	$(selector).clickover({
		content: $('#' + $(selector).attr('rel')).html(), onShown: function () {
			this.$tip.find('.popover-title').append('<button class="close" data-dismiss="clickover">&times;</button>');
		},
	});
}

function initToonWachtwoordToggle() {
	$('i.toonWachtwoordToggle').mouseleave((e) => {
		setWachtwoordVisibility(e, false)
	});

	$('i.toonWachtwoordToggle').mousedown((e) => {
		setWachtwoordVisibility(e, true)
	});

	$('i.toonWachtwoordToggle').mouseup((e) => {
		setWachtwoordVisibility(e, false)
	});

	function setWachtwoordVisibility(event, visible) {
		const element = $(event.target)
		element.attr('class', element.attr('class').replace(visible ? 'icon-eye-open' : 'icon-eye-close', visible ? 'icon-eye-close' : 'icon-eye-open'));
		element.next('input').attr('type', visible ? 'text' : 'password')
	}
}

function replaceTextNieuwsItem() {
	let nieuwsItemTekst = document.getElementsByClassName('js-nieuws-item-tekst');

	$(".js-nieuws-item-tekst").each(function (index) {
		$(".js-nieuws-item-tekst")[index].innerHTML.replace(/\n/g, "<br />");
	});
}

document.addEventListener('DOMContentLoaded', function () {
	replaceTextNieuwsItem();
	initToonWachtwoordToggle();
});
