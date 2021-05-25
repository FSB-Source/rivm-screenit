package nl.rivm.screenit.main.service.impl;

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

import java.util.List;

import nl.rivm.screenit.main.dao.BriefDao;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.BriefType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BriefServiceImpl implements BriefService
{

	@Autowired
	private BriefDao briefDao;

	@Override
	public <MB extends MergedBrieven<?>> List<MB> getMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter, long first, long count,
		String sortProperty, boolean ascending)
	{
		return briefDao.getMergedBrieven(screeningOrganisatie, filter, first, count, sortProperty, ascending);
	}

	@Override
	public <MB extends MergedBrieven<?>> Long countMergedBrieven(ScreeningOrganisatie screeningOrganisatie, MergedBrievenFilter<MB> filter)
	{
		return briefDao.countMergedBrieven(screeningOrganisatie, filter);
	}

	@Override
	public List<? extends ClientBrief> getBrievenVanAfmelding(Afmelding afmelding, boolean heraanmelding)
	{
		if (heraanmelding)
		{
			BriefType[] types = {
				BriefType.CERVIX_HERAANMELDING_AANVRAAG,
				BriefType.CERVIX_HERAANMELDING_HANDTEKENING,
				BriefType.CERVIX_HERAANMELDING_BEVESTIGING,
				BriefType.COLON_HERAANMELDING_AANVRAAG,
				BriefType.COLON_HERAANMELDING_HANDTEKENING,
				BriefType.COLON_HERAANMELDING_BEVESTIGING,
				BriefType.MAMMA_HERAANMELDING_BEVESTIGING
			};
			return briefDao.getBrievenVanAfmelding(afmelding, types);
		}
		else
		{
			BriefType[] types = {
				BriefType.CERVIX_AFMELDING_AANVRAAG,
				BriefType.CERVIX_AFMELDING_HANDTEKENING,
				BriefType.CERVIX_BEVESTIGING_DEFINITIEVE_AFMELDING,
				BriefType.COLON_AFMELDING_AANVRAAG,
				BriefType.COLON_AFMELDING_HANDTEKENING,
				BriefType.COLON_AFMELDING_BEVESTIGING,
				BriefType.MAMMA_AFMELDING_AANVRAAG,
				BriefType.MAMMA_AFMELDING_HANDTEKENING,
				BriefType.MAMMA_BEVESTIGING_DEFINITIEVE_AFMELDING
			};
			return briefDao.getBrievenVanAfmelding(afmelding, types);
		}
	}

	@Override
	public List<BezwaarBrief> getBrievenVanBezwaar(BezwaarMoment moment)
	{
		return briefDao.getBrievenVanBezwaar(moment);
	}
}
