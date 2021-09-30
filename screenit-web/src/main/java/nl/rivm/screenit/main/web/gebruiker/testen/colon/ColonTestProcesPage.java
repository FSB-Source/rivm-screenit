package nl.rivm.screenit.main.web.gebruiker.testen.colon;

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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.testen.TestTimeLineDossierTijdstip;
import nl.rivm.screenit.main.model.testen.TestTimelineModel;
import nl.rivm.screenit.main.service.TestTimelineService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.client.base.ClientLoginFoutPage;
import nl.rivm.screenit.main.web.client.dashboard.home.ClientHomeDashboardPage;
import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.TestService;
import nl.rivm.screenit.service.colon.ColonTestService;
import nl.rivm.screenit.util.TestBsnGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.enumdropdownchoice.EnumChoiceRenderer;
import nl.topicuszorg.wicket.input.validator.BSNValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.FileResourceStream;
import org.apache.wicket.util.resource.IResourceStream;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Restrictions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class ColonTestProcesPage extends TestenBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ColonTestProcesPage.class);

	@SpringBean
	private ColonTestService colonTestService;

	@SpringBean
	private TestService testService;

	@SpringBean
	private TestTimelineService testTimelineService;

	@SpringBean(name = "portaalUrl")
	private String clientportaalUrl;

	@SpringBean
	private ClientService clientService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private IModel<String> aantalBrieven = new Model<>("0");

	private IModel<String> bsns = new Model<>("");

	public ColonTestProcesPage()
	{
		addColoscopieTestForm();
	}

	private void addColoscopieTestForm()
	{
		GbaPersoon persoon = new GbaPersoon();
		persoon.setGeslacht(Geslacht.MAN);
		IModel<GbaPersoon> filterModel = ModelUtil.cModel(persoon);
		persoon = filterModel.getObject();
		persoon.setGbaAdres(new BagAdres());
		final Form<GbaPersoon> form = new Form<>("form", filterModel);
		final TextField<String> bsnField = new TextField<>("bsn");
		bsnField.add(new BSNValidator(true, true));
		bsnField.setRequired(true);
		bsnField.setOutputMarkupId(true);
		form.add(bsnField);
		form.add(new IndicatingAjaxLink<GbaPersoon>("bsnGenereren", filterModel)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				GbaPersoon object = getModelObject();
				object.setBsn(TestBsnGenerator.getValideBsn());
				target.add(bsnField);
			}
		});
		form.add(new ScreenitDateTextField("geboortedatum").setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		}));

		form.add(new ScreenitDropdown<Geslacht>("geslacht", Arrays.asList(Geslacht.values()), new EnumChoiceRenderer<Geslacht>()));

		form.add(new ScreenitDateTextField("overlijdensdatum").setOutputMarkupId(true).add(new AjaxFormComponentUpdatingBehavior("change")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(getComponent());
			}
		}));

		form.add(new DropDownChoice<Gemeente>("gbaAdres.gbaGemeente", ModelUtil.listRModel(
			hibernateService.getHibernateSession().createCriteria(Gemeente.class).add(Restrictions.isNotNull("screeningOrganisatie")).addOrder(Order.asc("naam")).list(), false),
			new ChoiceRenderer<Gemeente>("naam")));

		form.add(new AjaxButton("maakClient", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				testService.maakClient((GbaPersoon) form.getModelObject());
				info("Client aangemaakt");
			}
		});
		form.add(new AjaxButton("metConclusieColoscopie", form)
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.maakAfspraakEnConclusie((GbaPersoon) form.getModelObject(), null);
				info("Afspraak met coloscopie conclusie aangemaakt");
			}
		});
		form.add(new AjaxButton("afspraakAlleen", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.maakAfspraak((GbaPersoon) form.getModelObject(), null);
				info("Afspraak aangemaakt");
			}
		});

		form.add(new AjaxButton("directNaarClientDossier", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String bsn = ((GbaPersoon) form.getModelObject()).getBsn();
				Client client = clientService.getClientByBsn(bsn);
				if (client != null)
				{
					setResponsePage(new ClientInzienPage(new SimpleHibernateModel<>(client)));
				}
				else
				{
					error("Je client is nog niet bekend binnen ScreenIT. Maak eerst de client aan voordat je naar het dossier kunt gaan!");
				}
			}
		});
		form.add(new AjaxButton("directNaarOudeClientportaal", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ScreenitSession.get().setAfkomstigURLRegioCode(clientportaalUrl + "/");
				if (ScreenitSession.get().signIn(((GbaPersoon) form.getModelObject()).getBsn()))
				{
					setResponsePage(ClientHomeDashboardPage.class);
				}
				else
				{
					setResponsePage(new ClientLoginFoutPage(getString("loginUnsuccessfulErrorMessage")));
				}
			}
		});

		form.add(new AjaxButton("metIfobtOngunstig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				IFOBTTest test = colonTestService.maakHuidigeIFobtOntvangenEnOngunstig((GbaPersoon) form.getModelObject());
				info("Deze Client heeft nu een FIT teruggestuurd! Status Ifobttest is " + test.getStatus() + " met normwaarde: " + test.getNormWaarde() + " met uitslag: "
					+ test.getUitslag());
			}

		});
		form.add(new AjaxButton("metIfobtGunstig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				IFOBTTest test = colonTestService.maakHuidigeIFobtOntvangenEnGunstig((GbaPersoon) form.getModelObject());
				info("Deze Client heeft nu een FIT teruggestuurd! Status Ifobttest is " + test.getStatus() + " met normwaarde: " + test.getNormWaarde() + " met uitslag: "
					+ test.getUitslag());
			}

		});

		form.add(new AjaxButton("voorAfronden", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.maakClientKlaarVoorAfronden((GbaPersoon) form.getModelObject());
				info("Client klaar gezet voor afronden");
			}
		});

		form.add(new AjaxButton("HuidigeIFOBTvoorRapelDatum", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.huidigeIFOBTvoorRapelDatum((GbaPersoon) form.getModelObject());
				info("De huidige datum van FIT is voor de rapel datum gezet");
			}

		});

		form.add(new AjaxButton("VerlorenIfobtKrijgtUitslagGunstig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				IFOBTTest ifobt = colonTestService.zetVelorenIfobt((GbaPersoon) form.getModelObject(), Boolean.TRUE, Boolean.TRUE);
				info("FIT heeft een uitslag gekregen! Status: " + ifobt.getStatus().name());
			}

		});

		form.add(new AjaxButton("VerlorenIfobtKrijgtUitslagOngunstig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				IFOBTTest ifobt = colonTestService.zetVelorenIfobt((GbaPersoon) form.getModelObject(), Boolean.TRUE, Boolean.FALSE);
				info("FIT heeft een uitslag gekregen! Status: " + ifobt.getStatus().name());
			}

		});

		form.add(new AjaxButton("voorRappeleren", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				GbaPersoon gbaPersoon = (GbaPersoon) form.getModelObject();
				TestTimelineModel model = new TestTimelineModel();
				model.setBsn(gbaPersoon.getBsn());
				model.setGeslacht(gbaPersoon.getGeslacht());
				model.setGeboortedatum(gbaPersoon.getGeboortedatum());
				List<Client> clienten = testTimelineService.maakOfVindClienten(model);
				Client client = clienten.get(0);
				testTimelineService.maakNieuweScreeningRonde(client, TestTimeLineDossierTijdstip.DAG_NA_UITNODIGING_KOPPELEN, ColonOnderzoeksVariant.STANDAARD);
				int rapelDagen = preferenceService.getInteger(PreferenceKey.IFOBTRAPELPERIODE.name());
				testTimelineService.verzetDossierAchteruitInTijd(client, rapelDagen);
				info("Client klaar gezet voor rappeleren");
			}

		});

		form.add(new AjaxButton("haBerichtOngunstig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.huisartsBerichtKlaarzettten((GbaPersoon) form.getModelObject(), HuisartsBerichtType.ONGUNSTIGE_UITSLAG);
				info("HuisartsBericht klaar gezet");
			}

		});
		form.add(new AjaxButton("haBerichtWijzig", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.huisartsBerichtKlaarzettten((GbaPersoon) form.getModelObject(), HuisartsBerichtType.WIJZIGING_INTAKEAFSPRAAK);
				info("HuisartsBericht klaar gezet");
			}

		});
		form.add(new AjaxButton("haBerichtAnnuleer", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.huisartsBerichtKlaarzettten((GbaPersoon) form.getModelObject(), HuisartsBerichtType.ANNULEREN_INTAKEAFSPRAAK);
				info("HuisartsBericht klaar gezet");
			}
		});
		form.add(new AjaxButton("haBerichtNoShow", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				colonTestService.huisartsBerichtKlaarzettten((GbaPersoon) form.getModelObject(), HuisartsBerichtType.NO_SHOW_INTAKE);
				info("HuisartsBericht klaar gezet");
			}
		});

		final Form<GbaPersoon> form2 = new ScreenitForm<>("form2", filterModel);
		form2.add(new TextField<String>("bsn").setRequired(true));
		final AjaxDownload download = new AjaxDownload()
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected IResourceStream getResourceStream()
			{
				return createResourceStream(form2.getModelObject());
			}

			@Override
			protected String getFileName()
			{
				return "vo107.txt";
			}
		};
		form2.add(download);

		form2.add(new AjaxButton("downloadVo107", form2)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{

				download.initiate(target);
			}
		});

		Form<Void> form3 = new ScreenitForm<>("form3");
		form3.add(new TextField<>("aantalBrieven", aantalBrieven).setRequired(true));
		form3.add(new AjaxButton("brievenKlaarzetten", form3)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				int aantal = Integer.parseInt(aantalBrieven.getObject());
				colonTestService.brievenKlaarzetten(aantal);
				info("brieven zijn klaargezet;");
			}
		});

		Form<Void> form4 = new ScreenitForm<>("form4");
		form4.add(new TextArea<>("bsns", bsns).setRequired(true));
		form4.add(new AjaxButton("resetten", form4)
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				String message = colonTestService.clientenResetten(bsns.getObject());
				if (message.contains("Succesvol"))
				{
					info(message);
				}
				else
				{
					error(message);
				}
			}
		});

		WebMarkupContainer verwijderRoosterBlokkenContainer = new WebMarkupContainer("verwijderRoosterBlokkenContainer");

		verwijderRoosterBlokkenContainer.add(new AjaxLink<Void>("verwijderRoosterBlokken")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				int aantalRoosterBlokkenVerwijderd = colonTestService.verwijderRoosterBlokken();
				String infoBericht = "Er zijn " + Integer.toString(aantalRoosterBlokkenVerwijderd) + " roosterblokken verwijderd";
				info(infoBericht);
			}
		});

		add(form);
		add(form2);
		add(form3);
		add(form4);
		add(verwijderRoosterBlokkenContainer);

	}

	protected IResourceStream createResourceStream(GbaPersoon modelObject)
	{
		FileResourceStream fileOutputStream = null;
		try
		{
			File file = new File("vo107.txt");
			FileOutputStream stream = new FileOutputStream(file);
			File vo107template = new File(ColonTestProcesPage.class.getResource("/vo107_template.txt").getFile());
			testService.createGbaFile(modelObject, vo107template, stream);
			fileOutputStream = new FileResourceStream(file);
		}
		catch (FileNotFoundException e)
		{
			LOG.error("Er is een fout opgetreden!", e);
		}
		return fileOutputStream;

	}
}
