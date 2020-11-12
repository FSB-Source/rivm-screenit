package nl.rivm.screenit.mamma.se.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import nl.rivm.screenit.mamma.se.service.dtomapper.PlanningDtoMapper;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/planning")
public class PlanningController extends AuthorizedController
{
	@Autowired
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private PlanningDtoMapper planningDtoMapper = new PlanningDtoMapper();

	@RequestMapping(value = "/{datum}", method = RequestMethod.GET)
	public ResponseEntity getGeenScreeningCapaciteitBlokken(@PathVariable @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate datum, HttpServletRequest request)
	{
		MammaScreeningsEenheid screeningseenheid = getScreeningsEenheid(request);
		MammaStandplaatsPeriode standplaatsPeriode = createStandplaatsPeriode(datum, screeningseenheid);

		if (standplaatsPeriode != null)
		{
			Date date = DateUtil.toUtilDate(datum);
			Date eindDate = DateUtil.toUtilDate(datum.plusDays(1));

			List<MammaCapaciteitBlok> geenScreeningCapaciteitsBlokken = baseCapaciteitsBlokService
				.getCapaciteitsBlokken(standplaatsPeriode.getScreeningsEenheid(), date, eindDate, false, Arrays.asList(MammaCapaciteitBlokType.GEEN_SCREENING));

			return ResponseEntity.ok(planningDtoMapper.createPlanningSeDto(geenScreeningCapaciteitsBlokken));
		}
		else
		{
			return ResponseEntity.ok(planningDtoMapper.createPlanningSeDto(new ArrayList<>()));
		}
	}

	private MammaStandplaatsPeriode createStandplaatsPeriode(LocalDate daglijstDatum, MammaScreeningsEenheid screeningseenheid)
	{
		for (MammaStandplaatsPeriode standplaatsPeriode : screeningseenheid.getStandplaatsPerioden())
		{
			LocalDate van = DateUtil.toLocalDate(standplaatsPeriode.getVanaf());
			LocalDate totEnMet = DateUtil.toLocalDate(standplaatsPeriode.getTotEnMet());
			if (van.compareTo(daglijstDatum) <= 0 && totEnMet.compareTo(daglijstDatum) >= 0)
			{
				return standplaatsPeriode;
			}
		}
		return null;
	}
}
