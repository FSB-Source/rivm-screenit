package nl.rivm.screenit.main.web.gebruiker.testen;

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
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.dashboard.home.ClientHomeDashboardPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.testen.barcode.TestBarcodePage;
import nl.rivm.screenit.main.web.gebruiker.testen.cervix.timeline.CervixTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.clienten.verwijderen.ClientenVerwijderenPage;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.ColonTestPage;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.ColonTestProcesPage;
import nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.ColonTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.hpvbericht.TestHpvBerichtPage;
import nl.rivm.screenit.main.web.gebruiker.testen.hl7bericht.TestHL7BerichtPage;
import nl.rivm.screenit.main.web.gebruiker.testen.mamma.timeline.MammaTestTimelinePage;
import nl.rivm.screenit.main.web.gebruiker.testen.postcode.TestPostcodePage;
import nl.rivm.screenit.main.web.gebruiker.testen.preferences.TestPreferencesPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestenBasePage extends GebruikerBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ClientService clientService;

	protected WebMarkupContainer gebeurtenissenContainer;

	protected TextField<String> bsnField;

	public TestenBasePage()
	{

	}

	protected WebMarkupContainer getGebeurtenissenContainer()
	{
		return null;
	}

	protected void addClientBsnGenererenButtons(WebMarkupContainer container, IModel<TestTimelineModel> model)
	{
		container.add(new IndicatingAjaxLink<TestTimelineModel>("bsnGenereren", model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				TestTimelineModel object = model.getObject();
				object.setBsn(TestBsnGenerator.getValideBsn());
				target.add(bsnField);

				WebMarkupContainer geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				gebeurtenissenContainer.setVisible(false);
				target.add(gebeurtenissenContainer);

			}
		});

		container.add(new IndicatingAjaxLink<TestTimelineModel>("bsnToevoegen", model)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				TestTimelineModel object = model.getObject();
				if (!StringUtils.isEmpty(object.getBsnsString()))
				{
					object.setBsn(object.getBsnsString() + "," + TestBsnGenerator.getValideBsn());
				}
				else
				{
					object.setBsn(TestBsnGenerator.getValideBsn());
				}
				target.add(bsnField);

				WebMarkupContainer geContainer = getGebeurtenissenContainer();
				gebeurtenissenContainer.replaceWith(geContainer);
				gebeurtenissenContainer = geContainer;
				gebeurtenissenContainer.setVisible(false);
				target.add(gebeurtenissenContainer);

			}
		});
	}

	protected TestTimelineModel refreshTimelineModel(TestTimelineModel timelineModel, List<Client> clienten)
	{
		Client client = clienten.get(0);
		GbaPersoon persoon = client.getPersoon();
		timelineModel.setGeslacht(persoon.getGeslacht());
		timelineModel.setGeboortedatum(persoon.getGeboortedatum());
		timelineModel.setGemeente(persoon.getGbaAdres().getGbaGemeente());
		MammaDossier mammaDossier = client.getMammaDossier();
		if (mammaDossier != null)
		{
			timelineModel.setDoelgroep(mammaDossier.getDoelgroep());
			timelineModel.setDeelnamekans(mammaDossier.getDeelnamekans().getDeelnamekans());
		}
		timelineModel.setBsn(clienten.stream().map(c -> c.getPersoon().getBsn()).collect(Collectors.joining(",")));
		return timelineModel;
	}

	protected WebMarkupContainer getClientDossierButton(Form form, IModel<TestTimelineModel> model)
	{
		if (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_GEGEVENS, Actie.INZIEN))
		{
			return new AjaxButton("directNaarClientDossier", form)
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					if (model.getObject().getBsns().size() > 0)
					{
						Client client = clientService.getClientByBsn(model.getObject().getBsns().get(0));
						setResponsePage(new ClientInzienPage(new SimpleHibernateModel<>(client)));
					}
					else
					{
						error("Geen bsn gevonden.");
					}
				}
			};
		}
		else
		{
			return new EmptyPanel("directNaarClientDossier");
		}
	}

	protected AjaxButton getClientPortaalButton(Form form, String clientportaalUrl, IModel<TestTimelineModel> model)
	{
		return new AjaxButton("directNaarClientPortaal", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (model.getObject().getBsns().size() > 0)
				{
					ScreenitSession.get().setAfkomstigURLRegioCode(clientportaalUrl + "/");
					ScreenitSession.get().signIn(model.getObject().getBsns().get(0));
					setResponsePage(ClientHomeDashboardPage.class);
				}
				else
				{
					error("Geen bsn gevonden.");
				}
			}
		};
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.TESTEN;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return createContextMenu();
	}

	private List<GebruikerMenuItem> createContextMenu()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.bmhk.hpvbericht", TestHpvBerichtPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.colon.ifobtbericht", TestHL7BerichtPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.postcode.testen.tools", TestPostcodePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.barcode.testen.tools", TestBarcodePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.preferences", TestPreferencesPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline.bmhk", CervixTestTimelinePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline.bk", MammaTestTimelinePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.timeline", ColonTestTimelinePage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.colon", ColonTestProcesPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.colon.testen", ColonTestPage.class));
		contextMenuItems.add(new GebruikerMenuItem("menu.testen.clienten.verwijderen", ClientenVerwijderenPage.class));
		return contextMenuItems;
	}
}
