package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

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

import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.ConclusieTypeFilter;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_WERKLIJST_MISSENDE_MDL_VERSLAGEN,
	organisatieTypeScopes = OrganisatieType.COLOSCOPIECENTRUM,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonMissendeMdlVerslagenWerklijstPage extends WerklijstIntakePage
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ColonMissendeMdlVerslagenWerklijstPage()
	{
		super(AfspraakStatus.UITGEVOERD, "title.werklijst.missende");

		form.setVisible(true);
		bsnForm.setVisible(true);
		conclusieType.setVisible(false);
		bsnGebroortedatumForm.setVisible(false);
		datepickerVandaag.setVisible(false);
	}

	@Override
	protected List<ConclusieTypeFilter> getFilterOpties()
	{
		return new ArrayList<>();
	}

	@Override
	protected List<IColumn<ColonIntakeAfspraak, String>> getColumns()
	{
		List<IColumn<ColonIntakeAfspraak, String>> columns = new ArrayList<>();

		columns.add(new ClientColumn<>("client.persoon.achternaam", "client"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "client.persoon.bsn", "client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("client.persoon.geboortedatum", "client.persoon"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Intakeafspraak"), "startTime", "startTime"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Datum intake conclusie"), "conclusie.datum", "conclusie.datum"));
		columns.add(new DateTimePropertyColumn<>(Model.of("Datum coloscopie"), "conclusie.datumColoscopie", "conclusie.datumColoscopie", new SimpleDateFormat("dd-MM-yyyy")));

		return columns;
	}

	@Override
	protected IModel<WerklijstIntakeFilter> getNewWerkLijstIntakeFilter(AfspraakStatus afspraakStatus)
	{
		var intakeFilter = super.getNewWerkLijstIntakeFilter(afspraakStatus);
		var filter = intakeFilter.getObject();

		var gisteren = currentDateSupplier.getLocalDate().minusDays(1L);
		var start = gisteren.minusYears(2L);

		filter.setVanaf(DateUtil.toUtilDate(start));
		filter.setTotEnMet(DateUtil.toUtilDate(gisteren));

		return intakeFilter;
	}

	@Override
	protected SortableDataProvider<ColonIntakeAfspraak, String> getWerklijstIntakeDataProvider(ColoscopieCentrum intakelocatie, int aantalPerPagina)
	{
		return new ColonMissendeMdlVerslagenDataProvider(zoekModel, intakelocatie, aantalPerPagina);
	}

	@Override
	protected void addExtraValidators(FormComponent<Date> vanaf, FormComponent<Date> totEnMet)
	{
		var vandaag = currentDateSupplier.getLocalDate().minusDays(1L);
		totEnMet.add(DateValidator.maximum(Date.from(vandaag.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant())));
	}
}
