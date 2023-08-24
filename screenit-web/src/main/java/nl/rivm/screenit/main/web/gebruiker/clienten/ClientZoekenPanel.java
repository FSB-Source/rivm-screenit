
package nl.rivm.screenit.main.web.gebruiker.clienten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.util.postcode.PostcodeFormatter;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
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

public class ClientZoekenPanel extends Panel
{

	private static final Logger LOG = LoggerFactory.getLogger(ClientZoekenPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	private final IModel<String> briefkenmerkModel = new Model<>();

	private final WebMarkupContainer tabelContainer;

	public ClientZoekenPanel(String id)
	{
		super(id);
		Client zoekobject = new Client();
		GbaPersoon persoon = new GbaPersoon();
		persoon.setGbaAdres(new BagAdres());
		zoekobject.setPersoon(persoon);

		add(new ClientZoekenForm("form", new CompoundPropertyModel<>(zoekobject)));

		tabelContainer = new WebMarkupContainer("tabelContainer");
		tabelContainer.setOutputMarkupPlaceholderTag(true);
		tabelContainer.setVisible(false);
		tabelContainer.add(new WebMarkupContainer("tabel"));
		add(tabelContainer);
	}

	private void logAction(LogGebeurtenis gebeurtenis, Client client, String briefkenmerk)
	{
		String bsn = client.getPersoon().getBsn();
		if (bsn != null)
		{
			logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), "Gezocht op bsn: " + bsn);
		}
		else if (client.getPersoon().getGbaAdres() != null && client.getPersoon().getGbaAdres().getHuisnummer() != null)
		{
			logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(),
				"Gezocht op postcode + huisnummer: " + client.getPersoon().getGbaAdres().getPostcode() + " + " + client.getPersoon().getGbaAdres().getHuisnummer());
		}
		else if (briefkenmerk != null)
		{
			logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), "Gezocht op briefkenmerk: " + briefkenmerk);
		}
		else
		{
			String melding = "Gezocht op alleen geboortedatum.";
			LogEvent logEvent = new LogEvent(melding);
			logEvent.setLevel(Level.ERROR);
			logService.logGebeurtenis(gebeurtenis, logEvent, ScreenitSession.get().getLoggedInAccount(), null);
		}
	}

	private class ClientZoekenForm extends Form<Client>
	{

		private static final long serialVersionUID = 1L;

		@SpringBean
		private ClientService clientService;

		@SpringBean
		private HibernateService hibernateService;

		public ClientZoekenForm(String id, CompoundPropertyModel<Client> model)
		{
			super(id, model);

			add(new TextField<>("persoon.bsn").add(new BSNValidator()));

			add(new ScreenitDateTextField("persoon.geboortedatum").setRequired(true).setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.add(getComponent());
				}
			}));

			add(new PostcodeField("persoon.gbaAdres.postcode"));
			add(new TextField<>("persoon.gbaAdres.huisnummer"));

			add(new TextField<>("briefkenmerk", briefkenmerkModel));

			AjaxSubmitLink submit = new AjaxSubmitLink("submit")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					Client client = getModelObject();
					if (StringUtils.isNotBlank(client.getPersoon().getBsn())
						|| client.getPersoon().getGbaAdres().getHuisnummer() != null && StringUtils.isNotBlank(client.getPersoon().getGbaAdres().getPostcode()))
					{
						logAction(LogGebeurtenis.ZOEKEN_CLIENT, client, null);
						tabelContainer.setVisible(true);
						replaceTabel(target);
					}
					else if (StringUtils.isNotBlank(briefkenmerkModel.getObject()))
					{
						String errorString = clientService.valideerBriefkenmerk(briefkenmerkModel.getObject(), client);
						if (StringUtils.isBlank(errorString))
						{
							logAction(LogGebeurtenis.ZOEKEN_CLIENT, client, briefkenmerkModel.getObject());
							tabelContainer.setVisible(true);
							replaceTabel(target);
						}
						else
						{
							error(getString(errorString));
						}
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

				private static final long serialVersionUID = 1L;

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

			columns.add(new PropertyColumn<>(Model.of("Overlijdensdatum"), "persoon.overlijdensdatum", "persoon.overlijdensdatum"));

			columns.add(new PropertyColumn<Client, String>(Model.of("Postcode"), "persoon.gbaAdres.postcode", "persoon.gbaAdres.postcode")
			{

				private static final long serialVersionUID = 1L;

				@SuppressWarnings({ "unchecked", "rawtypes" })
				@Override
				public IModel<Object> getDataModel(IModel<Client> rowModel)
				{
					return new Model(PostcodeFormatter.formatPostcode((String) super.getDataModel(rowModel).getObject(), true));
				}

			});
			columns.add(new PropertyColumn<>(Model.of("Huisnummer"), "persoon.gbaAdres.huisnummer", "persoon.gbaAdres.huisnummer"));

			final ScreenitDataTable<Client, String> tabel = new ScreenitDataTable<Client, String>("tabel", columns, new SortableDataProvider<Client, String>()
			{

				private static final long serialVersionUID = 1L;

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

				private static final long serialVersionUID = 1L;

				@Override
				public void onClick(AjaxRequestTarget target, IModel<Client> model)
				{
					for (Object[] menuItem : ClientPage.CLIENT_DOSSIER_TABS)
					{
						Class<ClientPage> targetPageClass = (Class<ClientPage>) menuItem[1];
						if (Session.get().getAuthorizationStrategy().isInstantiationAuthorized(targetPageClass))
						{
							List<Object> params = new ArrayList<>();
							params.add(model);
							try
							{
								setResponsePage((Page) ConstructorUtils.invokeConstructor(targetPageClass, params.toArray()));
							}
							catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException | InstantiationException e)
							{
								LOG.error("Fout bij aanmaken van " + targetPageClass, e);
							}
							break;
						}
					}
				}
			};
			tabelContainer.addOrReplace(tabel);

		}

		private List<Client> getClienten()
		{
			List<Client> clienten = new ArrayList<>();
			Client zoekClient = ClientZoekenForm.this.getModelObject();
			GbaPersoon zoekPersoon = zoekClient.getPersoon();
			if (briefkenmerkModel.getObject() != null)
			{
				Client client = clientService.getClientMetBriefkenmerk(briefkenmerkModel.getObject());

				if (client != null)
				{
					GbaPersoon gevondenBriefkenmerkPersoon = client.getPersoon();

					if (gevondenBriefkenmerkPersoon.getGeboortedatumPrecisie() != null)
					{
						if (!DateUtil.isGeboortedatumGelijk(DateUtil.toLocalDate(zoekPersoon.getGeboortedatum()), client))
						{
							return clienten;
						}
					}
					else
					{
						return clienten;
					}

					if (zoekPersoon.getBsn() != null)
					{
						if (!zoekPersoon.getBsn().equals(gevondenBriefkenmerkPersoon.getBsn()))
						{
							return clienten;
						}
					}

					if (zoekPersoon.getGbaAdres().getPostcode() != null
						|| zoekPersoon.getGbaAdres().getHuisnummer() != null)
					{

						if (!(zoekPersoon.getGbaAdres().getPostcode() != null
							&& zoekPersoon.getGbaAdres().getHuisnummer() != null))
						{
							return clienten;
						}

						if (!(zoekPersoon.getGbaAdres().getPostcode().equals(gevondenBriefkenmerkPersoon.getGbaAdres().getPostcode())
							&& zoekPersoon.getGbaAdres().getHuisnummer().equals(gevondenBriefkenmerkPersoon.getGbaAdres().getHuisnummer())))
						{
							return clienten;
						}
					}

					clienten.add(client);
				}
				else
				{
					error(getString("error.geen.geldig.briefkenmerk"));
				}
			}
			else if (StringUtils.isNotBlank(zoekPersoon.getBsn()) || zoekPersoon.getGbaAdres().getHuisnummer() != null &&
				StringUtils.isNotBlank(zoekPersoon.getGbaAdres().getPostcode()))
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
