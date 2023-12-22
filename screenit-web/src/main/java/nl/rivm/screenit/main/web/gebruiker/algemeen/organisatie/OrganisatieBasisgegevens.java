package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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

import java.util.ArrayList;
import java.util.Map;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaCentraleEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.EmailAddressenValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker.MedewerkerZoeken;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.StringResourceModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.PatternValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_COLOSCOPIECENTRUM_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_MAMMAPOLI_ORG_BEHEER, Recht.GEBRUIKER_MAMMA_RADIOLOGIEAFDELING_ORG_BEHEER,
		Recht.GEBRUIKER_INPAKCENTRUM_ORG_BEHEER, Recht.GEBRUIKER_LABORATORIA_BEHEER, Recht.GEBRUIKER_PA_LABORATORIA_BEHEER, Recht.GEBRUIKER_RIVM_BEHEER,
		Recht.GEBRUIKER_ZORGVERZEKERAARS_BEHEER, Recht.GEBRUIKER_SCREENINGS_ORG_BEHEER, Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER, Recht.GEBRUIKER_COLOSCOPIELOCATIE_ORG_BEHEER,
		Recht.GEBRUIKER_HUISARTSENPRAKTIJKEN_BEHEER, Recht.GEBRUIKER_BMHK_LABORATORIA_BEHEER, Recht.GEBRUIKER_CENTRALE_EENHEID_ORG_BEHEER,
		Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER
	},
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class OrganisatieBasisgegevens extends OrganisatieBeheer
{

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	@SpringBean
	private MammaCentraleEenheidService centraleEenheidService;

	public OrganisatieBasisgegevens()
	{
		var currentSelectedOrganisatie = (Instelling) HibernateHelper.deproxy(getCurrentSelectedOrganisatie());
		init(ModelUtil.ccModel(currentSelectedOrganisatie));
	}

	public OrganisatieBasisgegevens(IModel<Instelling> model)
	{
		init(model);
	}

	private void init(IModel<Instelling> model)
	{
		var instelling = model.getObject();

		var label = new Label("label", Model.of("Bewerk organisatie"));

		if (instelling.getId() == null)
		{
			label = new Label("label", Model.of("Organisatie toevoegen"));
		}
		add(label);
		if (model.getObject().getId() != null)
		{
			add(new OrganisatiePaspoortPanel("paspoort", model));
		}
		else
		{
			add(new WebMarkupContainer("paspoort").setVisible(false));
		}
		add(new OrganisatieEditForm("organisatieForm", model));

	}

	public class OrganisatieEditForm extends ScreenitForm<Instelling>
	{

		public OrganisatieEditForm(String id, IModel<Instelling> model)
		{
			super(id, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			var organisatie = getModelObject();
			var adressen = organisatie.getAdressen();
			if (adressen == null)
			{
				organisatie.setAdressen(new ArrayList<>());
				adressen = organisatie.getAdressen(); 
			}
			if (adressen.isEmpty())
			{
				adressen.add(new Adres());
			}
			if (adressen.size() == 1)
			{
				adressen.add(new Adres());
			}
			if (adressen.size() == 2)
			{
				adressen.add(new Adres());
			}

			var organisatieType = organisatie.getOrganisatieType();
			var recht = organisatieType.getRecht();

			var actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie, recht);

			addOpslaanButton(actie);
			addVerwijderenButton(organisatie, actie);
			addAnnulerenButton();

			boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);
			boolean isSo = organisatieType.equals(OrganisatieType.SCREENINGSORGANISATIE);
			boolean isCe = organisatieType.equals(OrganisatieType.CENTRALE_EENHEID);
			boolean isSoOfCe = isSo || isCe;
			boolean isIntakeLocatie = organisatieType.equals(OrganisatieType.COLOSCOPIECENTRUM);
			var organisatieNaam = ComponentHelper.addTextField(this, "naam", true, 50, inzien).setLabel(Model.of("Naam"));

			organisatieNaam.add(new UniqueFieldValidator<>(Instelling.class, organisatie.getId(), "naam", hibernateService, Map.of("actief", Boolean.TRUE)));

			ComponentHelper.addTextField(this, "adressen[0].straat", false, 43, inzien);
			ComponentHelper.addTextField(this, "adressen[0].huisnummer", false, 10, Integer.class, inzien);
			ComponentHelper.addTextField(this, "adressen[0].huisnummerToevoeging", false, 26, inzien);
			ComponentHelper.newPostcodeTextField(this, "adressen[0].postcode", false, inzien).setLabel(Model.of("Postcode"));
			ComponentHelper.addTextField(this, "adressen[0].plaats", false, 200, inzien);

			ComponentHelper.addTextField(this, "adressen[1].huisnummer", isSoOfCe, 10, Integer.class, inzien);
			ComponentHelper.newPostcodeTextField(this, "adressen[1].postcode", isSoOfCe, inzien);
			ComponentHelper.addTextField(this, "adressen[1].plaats", isSoOfCe, 200, inzien);

			ComponentHelper.addTextField(this, "email", isSoOfCe, 100, inzien).add(EmailAddressValidator.getInstance());
			ComponentHelper.addTextField(this, "email2", isCe, 100, inzien).add(EmailAddressValidator.getInstance()).setVisible(isCe);
			ComponentHelper.addTextField(this, "email3", isCe, 100, inzien).add(EmailAddressValidator.getInstance()).setVisible(isCe);
			ComponentHelper.addTextField(this, "email4", isCe, 100, inzien).add(EmailAddressValidator.getInstance()).setVisible(isCe);
			ComponentHelper.addTextField(this, "emailSignaleringIntakelocatie", false, 100, inzien).add(EmailAddressenValidator.getInstance()).setVisible(isIntakeLocatie);
			ComponentHelper.addTextField(this, "website", false, 200, inzien).setVisible(!isCe);
			ComponentHelper.addTextField(this, "telefoon", isSoOfCe, 20, inzien);
			ComponentHelper.addTextField(this, "telefoon2", isCe, 20, inzien);
			ComponentHelper.addTextField(this, "telefoon3", isCe, 20, inzien).setVisible(isCe);
			ComponentHelper.addTextField(this, "telefoon4", isCe, 20, inzien).setVisible(isCe);
			ComponentHelper.addTextField(this, "fax", false, 200, inzien);

			ComponentHelper.addTextField(this, "adressen[2].huisnummer", isSoOfCe, 10, Integer.class, inzien).setVisible(isSoOfCe);
			ComponentHelper.newPostcodeTextField(this, "adressen[2].postcode", isSoOfCe, inzien).setVisible(isSoOfCe);
			ComponentHelper.addTextField(this, "adressen[2].plaats", isSoOfCe, 200, inzien).setVisible(isSoOfCe);

			var clientPortaalVrijeTekstContainer = new WebMarkupContainer("clientPortaalVrijeTekstContainer");
			clientPortaalVrijeTekstContainer.setOutputMarkupId(true);
			clientPortaalVrijeTekstContainer.setVisible(isSoOfCe);
			ComponentHelper.addTextArea(clientPortaalVrijeTekstContainer, "clientPortaalVrijeTekst", true, 255, inzien);
			add(clientPortaalVrijeTekstContainer);

			var gemachtigden = new ScreenitDropdown<>("gemachtigde", new SimpleListHibernateModel<>(instellingService.getActieveGebruikers(getModelObject())),
				new ChoiceRenderer<>("naamVolledig"));
			gemachtigden.setNullValid(true);
			gemachtigden.setEnabled(!inzien);
			gemachtigden.setVisible(!isCe);
			add(gemachtigden);

			add(new WebMarkupContainer("telNum1Label").setVisible(isSoOfCe));
			add(new WebMarkupContainer("telNum2LabelSo").setVisible(isSo));
			add(new WebMarkupContainer("telNum2LabelCe").setVisible(isCe));
			add(new Label("emailLabel", new StringResourceModel("mail.extra-label." + organisatieType.name())).setVisible(isCe));

			ComponentHelper.addTextField(this, "uziAbonneenummer", true, 8, inzien)
				.add(new UniqueFieldValidator<>(Instelling.class, organisatie.getId(), "uziAbonneenummer", hibernateService))
				.add(new PatternValidator("[0-9]{8}"))
				.setVisible(organisatieType == OrganisatieType.ZORGINSTELLING);

			var rootOid = ComponentHelper.addTextField(this, "rootOid", true, 255, String.class, inzien);
			rootOid.add(new PatternValidator(Constants.OID_EXTENSION_PATTERN));
			rootOid.add(new UniqueFieldValidator<>(Instelling.class, organisatie.getId(), "rootOid", hibernateService));
			rootOid.setVisible(
				organisatieType == OrganisatieType.COLOSCOPIELOCATIE || organisatieType == OrganisatieType.PA_LABORATORIUM || organisatieType == OrganisatieType.BMHK_LABORATORIUM);
		}

		private void addAnnulerenButton()
		{
			var annuleren = new AjaxLink<Gebruiker>("annuleren")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					setResponsePage(MedewerkerZoeken.class);
				}
			};
			add(annuleren);
			annuleren.setVisible(false);
		}

		private void addVerwijderenButton(Instelling organisatie, Actie actie)
		{
			var inActiveren = new ConfirmingIndicatingAjaxLink<Instelling>("inActiveren", dialog, "question.remove.organisatie")
			{

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					var organisatie = (Instelling) HibernateHelper.deproxy(OrganisatieEditForm.this.getModelObject());
					String feedbackMessageId = bepaalFeedback(organisatie);

					if (StringUtils.isNotBlank(feedbackMessageId))
					{
						error(getString(feedbackMessageId));
						return;
					}

					toggleActiefInactief(organisatie);

					setResponsePage(OrganisatieZoeken.class);
				}

				private void toggleActiefInactief(Instelling organisatie)
				{
					organisatie.setActief(Boolean.FALSE.equals(organisatie.getActief()));

					hibernateService.saveOrUpdate(organisatie);
					logAction(Boolean.FALSE.equals(organisatie.getActief()) ? LogGebeurtenis.ORGANISATIE_INACTIVEERD : LogGebeurtenis.ORGANISATIE_ACTIVEERD, organisatie);
				}

				private String bepaalFeedback(Instelling organisatie)
				{
					var feedbackMessageId = "";
					if (Boolean.TRUE.equals(organisatie.getActief()))
					{
						if (organisatie instanceof BeoordelingsEenheid)
						{
							var beoordelingsEenheid = (BeoordelingsEenheid) organisatie;
							feedbackMessageId = beoordelingsEenheidService.magWordenGeinactiveerd(beoordelingsEenheid);
						}
						else if (organisatie instanceof CentraleEenheid)
						{
							var centraleEenheid = (CentraleEenheid) organisatie;
							feedbackMessageId = centraleEenheidService.magWordenGeinactiveerd(centraleEenheid);
						}
					}
					else
					{
						if (organisatie instanceof BeoordelingsEenheid)
						{
							var beoordelingsEenheid = (BeoordelingsEenheid) organisatie;
							feedbackMessageId = beoordelingsEenheidService.magWordenGeactiveerd(beoordelingsEenheid);
						}
					}
					return feedbackMessageId;
				}

				@Override
				protected boolean skipConfirmation()
				{
					return Boolean.FALSE.equals(OrganisatieEditForm.this.getModelObject().getActief());
				}

			};

			inActiveren.add(new Label("inActiverenTitle", Boolean.FALSE.equals(organisatie.getActief()) ? "Activeren" : "Inactiveren"));
			inActiveren.setVisible(organisatie.getId() != null && isMinimumActie(actie, Actie.VERWIJDEREN) && !OrganisatieType.RIVM.equals(organisatie.getOrganisatieType()));
			add(inActiveren);
		}

		private void addOpslaanButton(Actie actie)
		{
			var opslaan = new ScreenitIndicatingAjaxSubmitLink("opslaan", this)
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					var organisatie = getModelObject();

					boolean nieuw = organisatie.getId() == null;
					instellingService.saveOrUpdate(organisatie);
					BasePage.markeerFormulierenOpgeslagen(target);
					if (nieuw)
					{
						logAction(LogGebeurtenis.ORGANISATIE_NIEUW, organisatie);
						setCurrentSelectedOrganisatie(organisatie);
						setResponsePage(new OrganisatieBasisgegevens(ModelUtil.ccModel(organisatie)));
					}
					else
					{
						logAction(LogGebeurtenis.ORGANISATIE_WIJZIG, organisatie);
					}

					String keyOpslaanGelukt = "action.save.organisatie";
					if (organisatie instanceof ColoscopieCentrum)
					{
						var intakelocatie = (ColoscopieCentrum) organisatie;
						if (intakelocatie.getPostcodeCoordinaten() == null)
						{
							ScreenitSession.get().warn(getString(keyOpslaanGelukt));
							ScreenitSession.get().warn(getString("intakelocatie.coordinaten.niet.gevonden.bij.adres"));
						}
						else
						{
							info(getString(keyOpslaanGelukt));
						}
					}
					else
					{
						info(getString(keyOpslaanGelukt));
					}
				}
			};
			opslaan.setVisible(isMinimumActie(actie, Actie.AANPASSEN));
			add(opslaan);
		}

		private void logAction(LogGebeurtenis gebeurtenis, Instelling organisatie)
		{
			logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), "Organisatie: " + organisatie.getNaam());
		}

	}

}
