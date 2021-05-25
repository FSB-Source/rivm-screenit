
package nl.rivm.screenit.main.service.mamma.impl;

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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.planning.PlanningStandplaatsPeriodeDto;
import nl.rivm.screenit.main.service.mamma.MammaRouteService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsPeriodeService;
import nl.rivm.screenit.main.util.ExportToXslUtil;
import nl.rivm.screenit.main.util.StandplaatsPeriodeUtil;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsOpmerking;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaRouteServiceImpl implements MammaRouteService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaStandplaatsPeriodeService standplaatsPeriodeService;

	@Autowired
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Override
	public void standplaatsenToevoegenRoute(MammaScreeningsEenheid screeningsEenheid, List<MammaStandplaats> standplaatsen, InstellingGebruiker ingelogdeInstellingGebruiker)
	{
		for (MammaStandplaats standplaats : standplaatsen)
		{
			PlanningStandplaatsPeriodeDto item = new PlanningStandplaatsPeriodeDto();
			item.standplaatsId = standplaats.getId();
			baseConceptPlanningsApplicatie.changeRoute(item, screeningsEenheid, ingelogdeInstellingGebruiker);
		}
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public String getCsvString(MammaScreeningsEenheid screeningsEenheid)
	{
		StringBuilder csvBuilder = new StringBuilder();

		String header = "Naam,Opmerkingen,Drempel,Start,Eind,Mindervalide uitnodigen vanaf,Interval,Indicatie,Locatie, Standplaatsomschrijving, Tijdelijke locatie, Standplaatsomschrijving (tijdelijke locatie), Start tijdelijke locatie, Eind tijdelijke locatie\n";
		csvBuilder.append(header);

		for (PlanningStandplaatsPeriodeDto standplaatsPeriode : standplaatsPeriodeService.getStandplaatsPeriodesSorted(screeningsEenheid))
		{
			MammaStandplaats standplaats = hibernateService.get(MammaStandplaats.class, standplaatsPeriode.standplaatsId);

			csvBuilder.append(ExportToXslUtil.getCsvValue(StandplaatsPeriodeUtil.getStandplaatsPeriodeNaam(standplaatsPeriode, standplaats)));
			csvBuilder.append(",");
			csvBuilder.append(
				ExportToXslUtil.getCsvValue(standplaats.getStandplaatsOpmerkingen().stream().filter(MammaStandplaatsOpmerking::getActief)
					.map(MammaStandplaatsOpmerking::getOpmerking).collect(Collectors.joining(";"))));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(standplaatsPeriode.afspraakDrempel != null ? standplaatsPeriode.afspraakDrempel + "%" : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(standplaatsPeriode.vanaf != null ? standplaatsPeriode.vanaf.format(DateUtil.LOCAL_DATE_FORMAT) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(standplaatsPeriode.totEnMet != null ? standplaatsPeriode.totEnMet.format(DateUtil.LOCAL_DATE_FORMAT) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil
				.getCsvValue(standplaatsPeriode.minderValideUitnodigenVanaf != null ? standplaatsPeriode.minderValideUitnodigenVanaf.format(DateUtil.LOCAL_DATE_FORMAT) : ""));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(ExportToXslUtil.getIntervalString(standplaatsPeriode.initieelIntervalMaanden)));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(standplaatsPeriode.meldingenDto.niveau.getIndicatieTekst()));
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(ExportToXslUtil.getStandplaatsLocatieString(standplaats.getLocatie())));
			csvBuilder.append(",");
			csvBuilder.append(standplaats.getLocatie().getLocatieBeschrijving() != null ? ExportToXslUtil.getCsvValue(standplaats.getLocatie().getLocatieBeschrijving()) : "");
			csvBuilder.append(",");
			csvBuilder.append(ExportToXslUtil.getCsvValue(ExportToXslUtil.getStandplaatsLocatieString(standplaats.getTijdelijkeLocatie())));
			csvBuilder.append(",");
			csvBuilder.append(
				standplaats.getTijdelijkeLocatie().getLocatieBeschrijving() != null ? ExportToXslUtil.getCsvValue(standplaats.getTijdelijkeLocatie().getLocatieBeschrijving())
					: "");
			csvBuilder.append(",");
			csvBuilder.append(standplaats.getTijdelijkeLocatie().getStartDatum() != null
				? ExportToXslUtil.getCsvValue(DateUtil.toLocalDate(standplaats.getTijdelijkeLocatie().getStartDatum()).format(DateUtil.LOCAL_DATE_FORMAT))
				: "");
			csvBuilder.append(",");
			csvBuilder.append(standplaats.getTijdelijkeLocatie().getEindDatum() != null
				? ExportToXslUtil.getCsvValue(DateUtil.toLocalDate(standplaats.getTijdelijkeLocatie().getEindDatum()).format(DateUtil.LOCAL_DATE_FORMAT))
				: "");

			csvBuilder.append("\n");
		}
		return csvBuilder.toString();
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<MammaStandplaats> getStandplaatsenZonderRoute(ScreeningOrganisatie screeningOrganisatie)
	{
		List<MammaStandplaats> standplaatsen = new ArrayList<>();
		for (Serializable standplaatsId : baseConceptPlanningsApplicatie.getStandplaatsenZonderRoute(screeningOrganisatie))
		{
			standplaatsen.add(hibernateService.get(MammaStandplaats.class, Long.valueOf(standplaatsId.toString())));
		}
		return standplaatsen;
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public List<MammaStandplaats> getStandplaatsenMetRoute(ScreeningOrganisatie screeningOrganisatie)
	{
		List<MammaStandplaats> standplaatsen = new ArrayList<>();
		for (Serializable standplaatsId : baseConceptPlanningsApplicatie.getStandplaatsenMetRoute(screeningOrganisatie))
		{
			standplaatsen.add(hibernateService.get(MammaStandplaats.class, Long.valueOf(standplaatsId.toString())));
		}
		return standplaatsen;
	}
}
