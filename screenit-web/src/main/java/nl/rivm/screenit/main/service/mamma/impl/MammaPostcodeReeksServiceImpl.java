package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.service.mamma.MammaPostcodeReeksService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.repository.mamma.MammaPostcodeReeksRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.filterNietId;
import static nl.rivm.screenit.specification.mamma.MammaPostcodeReeksSpecification.heeftOverlapMetReeks;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@RequiredArgsConstructor
public class MammaPostcodeReeksServiceImpl implements MammaPostcodeReeksService
{
	private final HibernateService hibernateService;

	private final LogService logService;

	private final MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	private final MammaPostcodeReeksRepository postcodeReeksRepository;

	@Override
	public boolean saveOrUpdatePostcodeReeks(MammaPostcodeReeks postcodeReeks, InstellingGebruiker loggedInInstellingGebruiker)
	{
		String melding = "";
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(postcodeReeks, hibernateService.getHibernateSession());

		boolean isNieuw = postcodeReeks.getId() == null;
		if (isNieuw)
		{
			melding += "Postcodereeks voor '" + postcodeReeks.getStandplaats().getNaam() + "' aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Postcodereeks voor '" + postcodeReeks.getStandplaats().getNaam() + "' gewijzigd (" + diffToLatestVersion + ").";
		}

		if (StringUtils.isNotEmpty(melding))
		{
			MammaStandplaats standplaats = postcodeReeks.getStandplaats();
			standplaats.getPostcodeReeksen().add(postcodeReeks);
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_POSTCODE_REEKS, loggedInInstellingGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdateAll(postcodeReeks, standplaats);
			baseConceptPlanningsApplicatie.sendPostcodeReeks(postcodeReeks, isNieuw);
			return true;
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String overlaptBestaandeReeks(MammaPostcodeReeks postcodeReeks)
	{
		var vanPostcode = postcodeReeks.getVanPostcode().toUpperCase();
		var totPostcode = postcodeReeks.getTotPostcode().toUpperCase();

		var postcodeRange = Range.closed(vanPostcode, totPostcode);

		return postcodeReeksRepository.findAll(heeftOverlapMetReeks(postcodeRange).and(filterNietId(postcodeReeks.getId()))).stream()
			.map(pr -> pr.getVanPostcode() + "->" + pr.getTotPostcode() + " in '" + pr.getStandplaats().getNaam()
				+ (Boolean.FALSE.equals(pr.getStandplaats().getActief()) ? " (inactief)" : "") + "'")
			.collect(Collectors.joining(","));
	}

	@Override
	public void deletePostcodeReeks(MammaPostcodeReeks postcodeReeks, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		String melding = "Postcodereeks voor '" + postcodeReeks.getStandplaats().getNaam() + "' verwijderd.";

		MammaStandplaats standplaats = postcodeReeks.getStandplaats();
		standplaats.getPostcodeReeksen().remove(postcodeReeks);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_POSTCODE_REEKS, ingelogdeInstellingGebruiker, melding, Bevolkingsonderzoek.MAMMA);
		baseConceptPlanningsApplicatie.deletePostcodeReeks(postcodeReeks);
		hibernateService.saveOrUpdate(standplaats);
		hibernateService.delete(postcodeReeks);
	}
}
