package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.onderbroken;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.mamma.beoordeling.MammaCeWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientContactActieTypeWrapper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.TelefoonnrColumn;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.springframework.beans.support.PropertyComparator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_ONDERBROKEN_ONDERZOEKEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeOnderbrokenOnderzoekenWerklijstPage extends AbstractMammaCeWerklijst
{
	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	private MammaCeOnderbrokenOnderzoekenDataProvider onderbrokenOnderzoekDataProvider;

	private boolean showCentraleEenheidSelector;

	public MammaCeOnderbrokenOnderzoekenWerklijstPage()
	{
		super();
		onderbrokenOnderzoekDataProvider = new MammaCeOnderbrokenOnderzoekenDataProvider("creatieDatum", zoekObjectModel);
		List<CentraleEenheid> alleMogelijkeCentraleEenheden = getAlleMogelijkeCentraleEenheden();
		showCentraleEenheidSelector = alleMogelijkeCentraleEenheden.size() > 1;
		MammaCeWerklijstZoekObject zoekObject = zoekObjectModel.getObject();
		if (zoekObject.getCentraleEenheden() == null)
		{
			zoekObject.setCentraleEenheden(alleMogelijkeCentraleEenheden);
			zoekObject.setScreeningsEenheden(getAlleMogelijkeScreeningsEenheden());
		}
		createResultTable();
		add(new ZoekForm("form", zoekObjectModel));
	}

	private List<CentraleEenheid> getAlleMogelijkeCentraleEenheden()
	{
		return instellingService.getMogelijkeCentraleEenheden(ScreenitSession.get().getScreeningOrganisatie());
	}

	private List<MammaScreeningsEenheid> getAlleMogelijkeScreeningsEenheden()
	{
		List<MammaScreeningsEenheid> mogelijkeScreeningsEenheden = new ArrayList<>();
		for (CentraleEenheid ce : zoekObjectModel.getObject().getCentraleEenheden())
		{
			for (BeoordelingsEenheid be : instellingService.getChildrenInstellingen(ce, BeoordelingsEenheid.class))
			{
				mogelijkeScreeningsEenheden.addAll(screeningsEenheidService.getActieveScreeningsEenhedenVoorBeoordelingsEenheid(be));
			}
		}
		Collections.sort(mogelijkeScreeningsEenheden, new PropertyComparator<>("code", true, true));
		return mogelijkeScreeningsEenheden;
	}

	class ZoekForm extends Form<MammaCeWerklijstZoekObject>
	{

		public ZoekForm(String id, IModel<MammaCeWerklijstZoekObject> model)
		{
			super(id, model);
			addCeSelector();
			addOrReplaceSeSelector();
			addBriefFilter();
			addZoekButton();
		}

		private void addBriefFilter()
		{
			add(new ScreenitDropdown<>("metBriefOproepOnderbrokenOnderzoek", Arrays.asList(Boolean.TRUE, Boolean.FALSE), new ChoiceRenderer<Boolean>()
			{
				@Override
				public Object getDisplayValue(Boolean object)
				{
					return object != null ? object ? "Ja" : "Nee" : "Alle";
				}
			}).setNullValid(true));
		}

		private void addOrReplaceSeSelector()
		{
			ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenheden = new ScreenitListMultipleChoice<>("screeningsEenheden",
				ModelUtil.listRModel(getAlleMogelijkeScreeningsEenheden()), new ChoiceRenderer<>("code"));
			screeningsEenheden.setOutputMarkupId(true);
			addOrReplace(screeningsEenheden);
		}

		private void addCeSelector()
		{
			ScreenitListMultipleChoice<CentraleEenheid> centraleEenhedenSelector = new ScreenitListMultipleChoice<>("centraleEenheden",
				ModelUtil.listRModel(getAlleMogelijkeCentraleEenheden()), new ChoiceRenderer<>("naam"));
			centraleEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					getModelObject().setScreeningsEenheden(getAlleMogelijkeScreeningsEenheden());
					addOrReplaceSeSelector();
					target.add(ZoekForm.this);
				}
			});
			centraleEenhedenSelector.setVisible(showCentraleEenheidSelector);

			add(centraleEenhedenSelector);
		}

		private void addZoekButton()
		{
			IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", this)
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					super.onSubmit(target);
					MammaCeWerklijstZoekObject zoekObject = getModelObject();
					if (CollectionUtils.isEmpty(zoekObject.getCentraleEenheden()))
					{
						zoekObject.setCentraleEenheden(getAlleMogelijkeCentraleEenheden());
					}
					if (CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
					{
						getModelObject().setScreeningsEenheden(getAlleMogelijkeScreeningsEenheden());
						addOrReplaceSeSelector();
					}
					target.add(ZoekForm.this);
					ScreenitSession.get().setZoekObject(getPageClass(), ZoekForm.this.getModel());
					target.add(resultatenContainer, ZoekForm.this);
				}
			};
			setDefaultButton(zoekenButton);
			add(zoekenButton);
		}

	}

	private void createResultTable()
	{
		List<IColumn<MammaOnderzoek, String>> columns = new ArrayList<>();
		columns.add(new DateTimePropertyColumn<>(Model.of("Onderzoeksdatum"), "creatieDatum", "creatieDatum",
			Constants.getDateTimeFormat()));
		columns.add(new ClientColumn<>("persoon.achternaam", "afspraak.uitnodiging.screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new TelefoonnrColumn<>("afspraak.uitnodiging.screeningRonde.dossier.client.persoon"));
		columns.add(new PropertyColumn<MammaOnderzoek, String>(Model.of("Beelden in IMS"), "mammografie.ilmStatus", "mammografie.ilmStatus")
		{
			@Override
			public void populateItem(Item<ICellPopulator<MammaOnderzoek>> item, String componentId, IModel<MammaOnderzoek> rowModel)
			{
				MammaMammografie mammografie = rowModel.getObject().getMammografie();
				MammaMammografieIlmStatus status = mammografie != null ? mammografie.getIlmStatus() : null;
				item.add(new Label(componentId, MammaMammografieIlmStatus.beeldenBeschikbaar(status) ? "Ja" : "Nee"));
			}
		});
		columns.add(new EnumPropertyColumn<>(Model.of("Reden"), "onderbrokenOnderzoek"));
		columns.add(new PropertyColumn<>(Model.of("SE"), "se.naam", "screeningsEenheid.code"));
		columns.add(new PropertyColumn<>(Model.of("CE"), "ce.naam", "screeningsEenheid.beoordelingsEenheid.parent.naam"));
		columns.add(new PropertyColumn<MammaOnderzoek, String>(Model.of("Datum brief oproep"), "afspraak.uitnodiging.screeningRonde")
		{

			@Override
			public IModel<?> getDataModel(IModel<MammaOnderzoek> rowModel)
			{
				MammaScreeningRonde ronde = (MammaScreeningRonde) super.getDataModel(rowModel).getObject();
				if (ronde.getLaatsteBrief().getBriefType() == BriefType.MAMMA_OPROEP_OPNEMEN_CONTACT)
				{
					return new Model(Constants.getDateFormat().format(ronde.getLaatsteBrief().getCreatieDatum()));
				}
				return new Model();
			}
		});

		resultatenContainer.add(new ScreenitDataTable<MammaOnderzoek, String>("resultaten", columns, onderbrokenOnderzoekDataProvider, 10, Model.of("onderbroken onderzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaOnderzoek> model)
			{
				super.onClick(target, model);
				List<Object> extraParameters = new ArrayList<>();
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_VANUIT_BK_PLANNING);
				extraParameters.add(Constants.CONTACT_EXTRA_PARAMETER_ALLEEN_CLIENT_CONTACT);
				ClientContactActieTypeWrapper actie = ClientContactActieTypeWrapper.MAMMA_AFSPRAAK_MAKEN;
				Client client = model.getObject().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient();
				setResponsePage(new ClientContactPage(ModelUtil.sModel(client), extraParameters, actie));
			}
		});
	}

}
