package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources;

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

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningWeekDto;
import nl.rivm.screenit.main.web.component.fullcalendar.event.EventSource;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.MammaCapaciteitOverviewPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.AantalRegulierOnderzoekenPerDagProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.AantalRegulierOnderzoekenPerWeekProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.BlokkadeEventsProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.StandplaatsEventsProvider;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit.sources.providers.WeekCapaciteitEventsProvider;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;

import org.apache.wicket.injection.Injector;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ScreenITEventSourceFactory implements Serializable
{

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private final IModel<MammaScreeningsEenheid> screeningsEenheidModel;

	private PlanningWeekDto weekDto;

	public ScreenITEventSourceFactory(IModel<MammaScreeningsEenheid> screeningsEenheid)
	{
		Injector.get().inject(this);
		screeningsEenheidModel = screeningsEenheid;
	}

	public EventSource getAantalOnderzoekenPerDagSource()
	{
		EventSource aantalOnderzoekenPerDag = new EventSource();
		aantalOnderzoekenPerDag.setBackgroundColor("#FFFFFF");
		aantalOnderzoekenPerDag.setBorderColor("#FFFFFF");
		aantalOnderzoekenPerDag.setEventsProvider(new AantalRegulierOnderzoekenPerDagProvider(this));
		return aantalOnderzoekenPerDag;
	}

	public EventSource getAantalOnderzoekenPerWeekSource()
	{
		EventSource aantalOnderzoekenPerWeek = new EventSource();
		aantalOnderzoekenPerWeek.setEventsProvider(new AantalRegulierOnderzoekenPerWeekProvider(this));
		return aantalOnderzoekenPerWeek;
	}

	public EventSource getWeekCapaciteitSource()
	{
		EventSource capaciteit = new EventSource();
		capaciteit.setEventsProvider(new WeekCapaciteitEventsProvider(this));
		return capaciteit;
	}

	public EventSource getBlokkadesSource()
	{
		EventSource blokkades = new EventSource();
		blokkades.setBackgroundColor("#f7e7e7");
		blokkades.setBorderColor("#df9f9f");
		blokkades.setEventsProvider(new BlokkadeEventsProvider(this));
		return blokkades;
	}

	public EventSource getStandplaatsSource(MammaCapaciteitOverviewPanel capaciteitOverviewPanel)
	{
		EventSource blokkades = new EventSource();
		blokkades.setBackgroundColor("#f7f2ff");
		blokkades.setBorderColor("#d8beff");
		blokkades.setEventsProvider(new StandplaatsEventsProvider(this, capaciteitOverviewPanel));
		return blokkades;
	}

	public void resetCapaciteit(Date weekStart)
	{
		MammaScreeningsEenheid screeningsEenheid = screeningsEenheidModel.getObject();
		weekDto = baseConceptPlanningsApplicatie.getWeek(screeningsEenheid, weekStart);

		LocalDateTime nu = dateSupplier.getLocalDateTime();
		if (nu.isAfter(DateUtil.toLocalDateTime(weekStart)))
		{
			Date to = Collections.min(Arrays.asList(DateUtil.toUtilDate(nu), DateUtil.plusDagen(weekStart, 7)));

			List<MammaCapaciteitBlok> blokken = baseCapaciteitsBlokService.getCapaciteitsBlokken(screeningsEenheid, weekStart, to, true,
				Arrays.asList(MammaCapaciteitBlokType.values()));

			final LocalDate prognoseVanafDatum = nu.toLocalTime().isBefore(Constants.BK_EINDTIJD_DAG) ? nu.toLocalDate() : nu.toLocalDate().plusDays(1);

			weekDto.dagen.stream().filter(dagDto -> dagDto.datum.isBefore(prognoseVanafDatum)).forEach(dagDto ->
			{
				final AtomicLong totaalAantalOnderzoeken = new AtomicLong();
				blokken.stream().filter(blok -> DateUtil.toLocalDate(blok.getVanaf()).equals(dagDto.datum)).forEach(blok ->
				{
					PlanningCapaciteitBlokDto blokDto = new PlanningCapaciteitBlokDto();
					blokDto.id = blok.getId();
					blokDto.blokType = blok.getBlokType();
					blokDto.vanaf = blok.getVanaf();
					blokDto.tot = blok.getTot();
					blokDto.screeningsEenheidId = screeningsEenheid.getId();
					blokDto.opmerkingen = blok.getOpmerkingen();
					blokDto.aantalOnderzoeken = blok.getAantalOnderzoeken();
					blokDto.minderValideAfspraakMogelijk = blok.getMinderValideAfspraakMogelijk();
					weekDto.blokken.add(blokDto);

					totaalAantalOnderzoeken.set(totaalAantalOnderzoeken.get() + blok.getBeschikbareCapaciteit().intValue());
				});
				dagDto.totaalAantalOnderzoeken = totaalAantalOnderzoeken.get();
			});
		}
	}

	public PlanningWeekDto getWeekDto()
	{
		return weekDto;
	}

	public PlanningCapaciteitBlokDto getBlok(UUID conceptId)
	{
		PlanningCapaciteitBlokDto blok = weekDto.blokken.stream().filter(b -> b.conceptId.equals(conceptId)).findFirst().orElse(null);
		if (blok != null)
		{
			return blok;
		}
		return null;
	}

	public IModel<MammaScreeningsEenheid> getScreeningsEenheidModel()
	{
		return screeningsEenheidModel;
	}
}
