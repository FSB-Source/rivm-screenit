package nl.rivm.screenit.model.enums;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import lombok.Getter;

import nl.rivm.screenit.service.MergeMailAttachmentService;
import nl.rivm.screenit.service.mamma.MammaMergeMailAttachmentService;
import nl.rivm.screenit.util.functionalinterfaces.BvoHouder;

@Getter
public enum DigitaalBerichtTemplateType implements BvoHouder
{

	MAMMA_AFSPRAAK_BEVESTIGING(DigitaalBerichtType.EMAIL, MammaMergeMailAttachmentService.class, Bevolkingsonderzoek.MAMMA)
		{
			@Override
			public List<MergeField> getMergeFields()
			{
				return List.of(MergeField.MAMMA_CE_TEL_INFOLIJN,
					MergeField.SO_WEBSITE,
					MergeField.MAMMA_CE_EMAIL_INFOLIJN,
					MergeField.CLIENT_AANHEF,
					MergeField.MAMMA_AFSPRAAK_BETREFT,
					MergeField.MAMMA_AFSPRAAK_REDEN_VERZET,
					MergeField.MAMMA_AFSPRAAK_DATUM,
					MergeField.MAMMA_AFSPRAAK_TIJD,
					MergeField.MAMMA_SP_STRAATNAAM,
					MergeField.MAMMA_SP_HUISNUMMER,
					MergeField.MAMMA_SP_POSTCODE,
					MergeField.MAMMA_SP_PLAATS,
					MergeField.MAMMA_SP_LOC_OMSCHRIJVING,
					MergeField.MAMMA_AFSPRAAK_LOCATIE_WIJZIGING,
					MergeField.MAMMA_CE_OPENINGSTIJDEN_TEKST,
					MergeField.SO_VERTEGENWOORDIGER,
					MergeField.MAMMA_UITNODIGINGSNUMMER,
					MergeField.MAMMA_UITNODIGINGSNUMMER_EMAIL_BARCODE,
					MergeField.SO_LOGO_EMAIL
				);
			}
		},
	MAMMA_AFSPRAAK_HERINNERING(DigitaalBerichtType.SMS, Bevolkingsonderzoek.MAMMA)
		{
			@Override
			public List<MergeField> getMergeFields()
			{
				return List.of(MergeField.MAMMA_AFSPRAAK_TIJD, MergeField.MAMMA_AFSPRAAK_DATUM, MergeField.MAMMA_SP_PLAATS);
			}
		};

	private final List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	private final DigitaalBerichtType berichtType;

	private final Class<? extends MergeMailAttachmentService> mergeMailAttachmentService;

	public abstract List<MergeField> getMergeFields();

	DigitaalBerichtTemplateType(DigitaalBerichtType berichtType, Class<? extends MergeMailAttachmentService> mergeMailAttachmentService,
		Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		this.berichtType = berichtType;
		this.mergeMailAttachmentService = mergeMailAttachmentService;
		this.bevolkingsonderzoeken = Arrays.asList(bevolkingsonderzoeken);
	}

	DigitaalBerichtTemplateType(DigitaalBerichtType berichtType, Bevolkingsonderzoek... bevolkingsonderzoeken)
	{
		this(berichtType, null, bevolkingsonderzoeken);
	}
}
