package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.beelden;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MammaBeeldenZoekenPanel extends Panel
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaBeeldenZoekenPanel.class);

	@SpringBean
	private LogService logService;

	@SpringBean
	private MammaBaseOnderzoekService onderzoekService;

	private final WebMarkupContainer tabelContainer;

	public MammaBeeldenZoekenPanel(String id)
	{
		super(id);
		Client zoekobject = new Client();
		GbaPersoon persoon = new GbaPersoon();
		persoon.setGbaAdres(new BagAdres());
		zoekobject.setPersoon(persoon);

		add(new MammaBeeldenZoekenPanel.MammaClientZoekenForm("form", new CompoundPropertyModel<>(zoekobject)));

		tabelContainer = new WebMarkupContainer("tabelContainer");
		tabelContainer.setOutputMarkupPlaceholderTag(true);
		tabelContainer.setVisible(false);
		tabelContainer.add(new WebMarkupContainer("tabel"));
		add(tabelContainer);
	}

	private class MammaClientZoekenForm extends Form<Client>
	{
		@SpringBean
		private ClientService clientService;

		public MammaClientZoekenForm(String id, CompoundPropertyModel<Client> model)
		{
			super(id, model);

			add(new TextField<>("persoon.bsn").setRequired(true).add(new BSNValidator()));

			add(new ScreenitDateTextField("persoon.geboortedatum").setRequired(true).setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.add(getComponent());
				}
			}));

			IndicatingAjaxSubmitLink submit = new IndicatingAjaxSubmitLink("submit")
			{

				protected void onSubmit(AjaxRequestTarget target)
				{
					Client client = getModelObject();
					if (StringUtils.isNotBlank(client.getPersoon().getBsn()))
					{
						String logRegel = String.format("Gezocht op bsn: %s en geboortedatum: %s",
							client.getPersoon().getBsn(),
							DateUtil.LOCAL_DATE_FORMAT.format(DateUtil.toLocalDate(client.getPersoon().getGeboortedatum())));

						logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_BEELDEN_CLIENT, ScreenitSession.get().getLoggedInAccount(), logRegel);
						tabelContainer.setVisible(true);
						replaceTabel(target);
					}
					else
					{
						client.getPersoon().setBsn("0");
						error(getString("error.ontbrekende.criteria"));
					}
					target.add(tabelContainer);
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
			columns.add(new PropertyColumn<>(Model.of("Bsn"), "persoon.bsn", "persoon.bsn"));
			columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "persoon"));

			final ScreenitDataTable<Client, String> tabel = new ScreenitDataTable<Client, String>("tabel", columns, new SortableDataProvider<Client, String>()
			{
				@Override
				public Iterator<? extends Client> iterator(long first, long count)
				{
					List<Client> clienten = getClienten();
					return clienten.iterator();
				}

				@Override
				public long size()
				{
					return getClienten().size();
				}

				@Override
				public IModel<Client> model(Client object)
				{
					return new SimpleHibernateModel<>(object);
				}
			}, Model.of("client(en)"))
			{
				@Override
				public void onClick(AjaxRequestTarget target, IModel<Client> model)
				{
					List<MammaOnderzoek> onderzoekenMetBeelden = onderzoekService.getOnderzoekenMetBeelden(model.getObject());
					if (!onderzoekenMetBeelden.isEmpty())
					{
						setResponsePage(new MammaBeeldenInzienPage(Arrays.asList(model.getObject().getId()), onderzoekenMetBeelden, MammaBeeldenZoekenPage.class));
						logService.logGebeurtenis(LogGebeurtenis.INZIEN_BEELDEN_CLIENT, ScreenitSession.get().getLoggedInAccount(), model.getObject());
					}
					else
					{
						error("Geen onderzoeken met beelden gevonden voor " + model.getObject().getName() + ".");
					}
				}
			};
			tabelContainer.addOrReplace(tabel);

		}

		private List<Client> getClienten()
		{
			List<Client> clienten = new ArrayList<>();
			Client zoekClient = MammaBeeldenZoekenPanel.MammaClientZoekenForm.this.getModelObject();
			GbaPersoon zoekPersoon = zoekClient.getPersoon();
			if (StringUtils.isNotBlank(zoekPersoon.getBsn()) && zoekPersoon.getGeboortedatum() != null)
			{
				clienten = clientService.zoekClienten(zoekClient);
			}
			else
			{
				error(getString("error.ontbrekende.criteria"));
			}
			return clienten;
		}

	}

}
