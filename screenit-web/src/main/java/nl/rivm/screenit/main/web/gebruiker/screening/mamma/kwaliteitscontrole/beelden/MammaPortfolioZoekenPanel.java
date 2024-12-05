package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaPortfolioZoekObject;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.service.mamma.impl.MammaPortfolioDataProviderServiceImpl;
import nl.rivm.screenit.main.util.WicketSpringDataUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

import static nl.rivm.screenit.util.StringUtil.propertyChain;

public class MammaPortfolioZoekenPanel extends Panel
{
	@SpringBean
	private LogService logService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private MammaBaseOnderzoekService onderzoekService;

	@SpringBean
	private MammaPortfolioDataProviderServiceImpl dataProviderService;

	private final WebMarkupContainer tabelContainer;

	private IModel<MammaPortfolioZoekObject> zoekObjectModel = new CompoundPropertyModel<>(new MammaPortfolioZoekObject());

	public MammaPortfolioZoekenPanel(String id)
	{
		super(id);

		if (ScreenitSession.get().isZoekObjectGezetForComponent(MammaPortfolioZoekenPage.class))
		{
			zoekObjectModel = (IModel<MammaPortfolioZoekObject>) ScreenitSession.get().getZoekObject(MammaPortfolioZoekenPage.class);
		}
		else
		{
			zoekObjectModel.getObject().setMammobridgeRol(MammobridgeRole.MBB);
		}

		add(new MammaPortfolioZoekenForm("form", zoekObjectModel));

		tabelContainer = new WebMarkupContainer("tabelContainer");
		tabelContainer.setOutputMarkupPlaceholderTag(true);
		tabelContainer.setVisible(false);
		tabelContainer.add(new WebMarkupContainer("tabel"));
		add(tabelContainer);
	}

	private class MammaPortfolioZoekenForm extends Form<MammaPortfolioZoekObject>
	{
		private MammaPortfolioZoekenForm(String id, IModel<MammaPortfolioZoekObject> model)
		{
			super(id, model);

			List<Gebruiker> gebruikers = medewerkerService.getActieveGebruikersMetRecht(Recht.GEBRUIKER_SCREENING_MAMMA_SE_ONDERZOEK);

			add(new ScreenitListMultipleChoice<>("gebruikers", ModelUtil.listRModel(gebruikers, false),
				new ChoiceRenderer<>()
				{
					@Override
					public Object getDisplayValue(Gebruiker gebruiker)
					{
						return NaamUtil.getNaamGebruiker(gebruiker);
					}
				}).setRequired(true));
			add(new DropDownChoice<>("mammobridgeRol", Arrays.asList(MammobridgeRole.values()),
				new EnumChoiceRenderer<>()).setRequired(true));
			var vanaf = ComponentHelper.monthYearDatePicker("vanaf").setRequired(true);
			add(vanaf);
			var totEnMet = ComponentHelper.monthYearDatePicker("totEnMet").setRequired(true);
			add(totEnMet);
			add(new DependantDateValidator(vanaf, totEnMet, DependantDateValidator.Operator.AFTER));

			IndicatingAjaxSubmitLink submit = new IndicatingAjaxSubmitLink("submit")
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					List<String> namen = NaamUtil.getNamenGebruikers(zoekObjectModel.getObject().getGebruikers());

					String logRegel = String.format("Gezocht op medewerker(s): %s van %s t/m %s als %s",
						String.join(", ", namen),
						DateUtil.LOCAL_DATE_FORMAT.format(DateUtil.toLocalDate(zoekObjectModel.getObject().getVanaf())),
						DateUtil.LOCAL_DATE_FORMAT.format(DateUtil.toLocalDate(zoekObjectModel.getObject().getTotEnMet())),
						zoekObjectModel.getObject().getMammobridgeRol().toString());

					logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_BEELDEN_PORTFOLIO, ScreenitSession.get().getLoggedInAccount(), logRegel);

					tabelContainer.setVisible(true);
					replaceTabel(target);
					target.add(tabelContainer);
					ScreenitSession.get().setZoekObject(MammaPortfolioZoekenPage.class, getModel());
				}

			};

			add(submit);
			this.setDefaultButton(submit);
		}

		private void replaceTabel(AjaxRequestTarget target)
		{
			List<IColumn<Client, String>> columns = new ArrayList<>();
			columns.add(new PropertyColumn<Client, String>(Model.of("Naam"), "persoon.achternaam", "persoon.achternaam")
			{

				@SuppressWarnings({ "rawtypes", "unchecked" })
				@Override
				public IModel<Object> getDataModel(IModel<Client> rowModel)
				{
					Client persoon = rowModel.getObject();
					String naam = NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(persoon);
					return new Model(naam);
				}

			});
			columns.add(new PropertyColumn<>(Model.of("Bsn"), propertyChain(Client_.PERSOON, GbaPersoon_.BSN), "persoon.bsn"));
			columns.add(new GeboortedatumColumn<>(propertyChain(Client_.PERSOON, GbaPersoon_.GEBOORTEDATUM), "persoon"));
			columns.add(new PropertyColumn(Model.of("Laatste mammografie"), propertyChain(Client_.MAMMA_DOSSIER, MammaDossier_.LAATSTE_MAMMOGRAFIE_AFGEROND),
				"mammaDossier.laatsteMammografieAfgerond"));

			MammaPortfolioDataProvider mammaPortfolioDataProvider = new MammaPortfolioDataProvider("mammaDossier.laatsteMammografieAfgerond", getModel());
			final ScreenitDataTable<Client, String> tabel = new ScreenitDataTable<Client, String>("tabel", columns,
				mammaPortfolioDataProvider, Model.of("client(en)"))
			{
				@Override
				public void onClick(AjaxRequestTarget target, IModel<Client> model)
				{
					List<MammaOnderzoek> onderzoekenMetBeelden = onderzoekService.getOnderzoekenMetBeelden(model.getObject());

					List<Long> clientenIds = dataProviderService.zoekPortfolioClientenIds(zoekObjectModel.getObject(),
						WicketSpringDataUtil.toSpringSort(mammaPortfolioDataProvider.getSort()));
					clientenIds.subList(0, clientenIds.indexOf(model.getObject().getId())).clear();
					setResponsePage(new MammaBeeldenInzienPage(clientenIds, onderzoekenMetBeelden, MammaPortfolioZoekenPage.class));
					logService.logGebeurtenis(LogGebeurtenis.INZIEN_BEELDEN_PORTFOLIO, ScreenitSession.get().getLoggedInAccount(), model.getObject());
					((MammaScreeningBasePage) getPage()).wijzigIDS7Role(zoekObjectModel.getObject().getMammobridgeRol());
				}
			};
			tabelContainer.addOrReplace(tabel);

		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(zoekObjectModel);
	}
}
