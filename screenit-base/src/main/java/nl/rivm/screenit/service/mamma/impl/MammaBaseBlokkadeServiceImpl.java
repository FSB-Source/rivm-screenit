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
import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.repository.mamma.MammaBaseBlokkadeRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBlokkadeService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.filterOpStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.heeftOverlapMet;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.heeftScreeningsEenheid;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.heeftScreeningsOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.heeftZelfdeScreeningsEenheidRegioOfStandplaats;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.isActief;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.isGeldigOp;
import static nl.rivm.screenit.specification.mamma.MammaBaseBlokkadeSpecification.isNietBlokkade;

@Service
public class MammaBaseBlokkadeServiceImpl implements MammaBaseBlokkadeService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@Autowired
	private MammaBaseBlokkadeRepository blokkadeRepository;

	@Autowired
	private LogService logService;

	@Override
	public List<MammaBlokkade> getOverlappendeBlokkadesVanZelfdeType(MammaBlokkade blokkade)
	{
		return blokkadeRepository.findAll(heeftOverlapMet(blokkade)
			.and(heeftZelfdeScreeningsEenheidRegioOfStandplaats(blokkade))
			.and(isActief(blokkade.getActief()))
			.and(isNietBlokkade(blokkade)));
	}

	@Override
	public List<MammaBlokkade> getActieveBlokkadesVoorSE(MammaStandplaats standplaats, MammaScreeningsEenheid screeningsEenheid, LocalDate dag)
	{
		return blokkadeRepository.findAll(isActief(true)
			.and(isGeldigOp(dag))
			.and(heeftScreeningsEenheid(screeningsEenheid)
				.or(heeftScreeningsOrganisatie((ScreeningOrganisatie) Hibernate.unproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio())))
				.or(filterOpStandplaats(standplaats))));
	}

	@Override
	@Transactional
	public void saveOrUpdate(MammaBlokkade blokkade, InstellingGebruiker ingelogdeGebruiker)
	{
		var melding = "";
		var diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(blokkade, hibernateService.getHibernateSession());

		var isNieuw = blokkade.getId() == null;
		var blokkadeNaam = "";
		if (blokkade.getScreeningsEenheid() != null)
		{
			blokkadeNaam = "screeningseenheid " + blokkade.getScreeningsEenheid().getNaam();
		}
		else if (blokkade.getStandplaats() != null)
		{
			blokkadeNaam = "standplaats " + blokkade.getStandplaats().getNaam();
		}
		else if (blokkade.getRegio() != null)
		{
			blokkadeNaam = "screeningsorganisatie " + blokkade.getRegio().getNaam();
		}
		if (isNieuw)
		{
			melding += "Blokkade '" + blokkadeNaam + "' aangemaakt.";
		}
		else if (!diffToLatestVersion.isEmpty())
		{
			melding += "Blokkade '" + blokkadeNaam + "' gewijzigd (" + diffToLatestVersion + ").";
		}
		if (StringUtils.isNotBlank(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_BLOKKADE, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			blokkadeRepository.save(blokkade);
			conceptPlanningsApplicatie.sendBlokkade(blokkade, isNieuw);
		}
	}

}
