package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.List;

import nl.rivm.screenit.dao.mamma.MammaBaseBlokkadeDao;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseBlokkadeService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class MammaBaseBlokkadeServiceImpl implements MammaBaseBlokkadeService
{

	@Autowired
	private MammaBaseBlokkadeDao blokkadeDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@Autowired
	private LogService logService;

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<MammaBlokkade> getOverlappendeBlokkades(MammaBlokkade blokkade)
	{
		return blokkadeDao.getOverlappendeBlokkades(blokkade);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdate(MammaBlokkade blokkade, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(blokkade, hibernateService.getHibernateSession());

		boolean isNieuw = blokkade.getId() == null;
		String blokkadeNaam = "";
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
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Blokkade '" + blokkadeNaam + "' gewijzigd (" + diffToLatestVersion + ").";
		}
		if (StringUtils.isNotBlank(melding))
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_BLOKKADE, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(blokkade);
			conceptPlanningsApplicatie.sendBlokkade(blokkade, isNieuw);
		}
	}

}
