package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.mamma.MammaBaseFollowUpDao;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingRadiologieDto;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SortState;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaBaseFollowUpServiceImpl implements MammaBaseFollowUpService
{

	@Autowired
	private MammaBaseFollowUpDao followUpDao;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public List<MammaFollowUpInstellingRadiologieDto> zoekOpenstaandeRadiologieVerslagenPerOrganisatie(ScreeningOrganisatie regio,
		MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie, Integer jaar)
	{
		return followUpDao.zoekOpenstaandeRadiologieVerslagenPerOrganisatie(regio, doorverwezenFilterOptie, jaar);
	}

	@Override
	public List<MammaFollowUpInstellingDto> zoekInstellingenMetOpenstaandePaVerslagen(ScreeningOrganisatie regio)
	{
		return followUpDao.zoekInstellingenMetOpenstaandePaVerslagen(regio);
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> zoekDossiersMetOpenstaandePaVerslagen(Instelling instelling, int first, int count, SortState<String> sortState)
	{
		return followUpDao.zoekDossiersMetOpenstaandePaVerslagen(instelling, first, count, sortState);
	}

	@Override
	public long countDossiersMetOpenstaandePaVerslagen(Instelling instelling)
	{
		return followUpDao.countDossiersMetOpenstaandePaVerslagen(instelling);
	}

	@Override
	public List<MammaBeoordeling> zoekOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio, int first, int count, SortState<String> sortState)
	{
		return followUpDao.zoekOpenstaandeFollowUpConclusies(regio, first, count, sortState);
	}

	@Override
	public long countOpenstaandeFollowUpConclusies(ScreeningOrganisatie regio)
	{
		return followUpDao.countOpenstaandeFollowUpConclusies(regio);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void refreshUpdateFollowUpConclusie(MammaDossier dossier)
	{
		Boolean heeftOpenstaandeFollowUpConclusie = followUpDao.heeftOpenstaandeFollowUpConclusie(dossier);
		if (!dossier.getUpdateFollowUpConclusie().equals(heeftOpenstaandeFollowUpConclusie))
		{
			dossier.setUpdateFollowUpConclusie(heeftOpenstaandeFollowUpConclusie);
			hibernateService.saveOrUpdate(dossier);
		}
	}

	@Override
	public LocalDate getEersteAutorisatieDatumPaVerslag(MammaScreeningRonde screeningRonde)
	{
		return getFollowUpVerslagenZonderLandelijkeMonitor(screeningRonde)
			.stream()
			.filter(v -> v.getVerslagContent() != null && v.getVerslagContent().getPathologieMedischeObservatie() != null
				&& v.getVerslagContent().getPathologieMedischeObservatie().getDatumAutorisatieUitslag() != null)
			.map(v -> v.getVerslagContent().getPathologieMedischeObservatie().getDatumAutorisatieUitslag())
			.map(DateUtil::toLocalDate)
			.min(Comparator.naturalOrder())
			.orElse(null);
	}

	@Override
	public List<MammaFollowUpVerslag> getFollowUpVerslagenZonderLandelijkeMonitor(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde.getFollowUpVerslagen().stream().filter(pa -> pa.getType() != VerslagType.MAMMA_PA_FOLLOW_UP_MONITOR).collect(Collectors.toList());
	}
}
