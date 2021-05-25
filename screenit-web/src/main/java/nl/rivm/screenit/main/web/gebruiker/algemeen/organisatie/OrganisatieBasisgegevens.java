package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie;

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
import java.util.HashMap;
import java.util.List;
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
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
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
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.TextField;
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

	private static final long serialVersionUID = 1L;

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

	private BootstrapDialog dialog;

	public OrganisatieBasisgegevens()
	{
		Instelling currentSelectedOrganisatie = (Instelling) HibernateHelper.deproxy(getCurrentSelectedOrganisatie());
		init(ModelUtil.cModel(currentSelectedOrganisatie));
	}

	public OrganisatieBasisgegevens(IModel<Instelling> model)
	{
		init(model);
	}

	private void init(IModel<Instelling> model)
	{
		Instelling instelling = model.getObject();

		Label label = new Label("label", Model.of("Bewerk organisatie"));

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
		dialog = new BootstrapDialog("dialog");
		add(dialog);

	}

	private void logAction(LogGebeurtenis gebeurtenis, Instelling organisatie)
	{
		logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), "Organisatie: " + organisatie.getNaam());
	}

	public class OrganisatieEditForm extends ScreenitForm<Instelling>
	{

		private static final long serialVersionUID = 1L;

		public OrganisatieEditForm(String id, IModel<Instelling> model)
		{
			super(id, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			Instelling instelling = getModelObject();
			if (instelling.getAdressen() == null)
			{
				instelling.setAdressen(new ArrayList<Adres>());
			}
			if (instelling.getAdressen().size() == 0)
			{
				instelling.getAdressen().add(new Adres());
			}
			if (instelling.getAdressen().size() == 1)
			{
				instelling.getAdressen().add(new Adres());
			}
			if (instelling.getAdressen().size() == 2)
			{
				instelling.getAdressen().add(new Adres());
			}

			OrganisatieType organisatieType = instelling.getOrganisatieType();
			Recht recht = organisatieType.getRecht();

			Actie actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), instelling, recht);

			addOpslaanButton(actie);
			addVerwijderenButton(instelling, actie);
			addAnnulerenButton();

			boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);
			boolean isSo = organisatieType.equals(OrganisatieType.SCREENINGSORGANISATIE);
			boolean isCe = organisatieType.equals(OrganisatieType.CENTRALE_EENHEID);
			boolean isSoOfCe = isSo || isCe;
			FormComponent<String> organisatieNaam = ComponentHelper.addTextField(this, "naam", true, 50, inzien).setLabel(Model.of("Naam"));

			Map<String, Object> restrictions = new HashMap<String, Object>();
			restrictions.put("actief", Boolean.TRUE);

			organisatieNaam.add(new UniqueFieldValidator<Instelling, String>(Instelling.class, instelling.getId(), "naam", hibernateService, restrictions));

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
			ComponentHelper.addTextField(this, "website", false, 200, inzien).setVisible(!isCe);
			ComponentHelper.addTextField(this, "telefoon", isSoOfCe, 20, inzien);
			ComponentHelper.addTextField(this, "telefoon2", isCe, 20, inzien);
			ComponentHelper.addTextField(this, "telefoon3", isCe, 20, inzien).setVisible(isCe);
			ComponentHelper.addTextField(this, "telefoon4", isCe, 20, inzien).setVisible(isCe);
			ComponentHelper.addTextField(this, "fax", false, 200, inzien);

			ComponentHelper.addTextField(this, "adressen[2].huisnummer", isSoOfCe, 10, Integer.class, inzien).setVisible(isSoOfCe);
			ComponentHelper.newPostcodeTextField(this, "adressen[2].postcode", isSoOfCe, inzien).setVisible(isSoOfCe);
			ComponentHelper.addTextField(this, "adressen[2].plaats", isSoOfCe, 200, inzien).setVisible(isSoOfCe);

			WebMarkupContainer clientPortaalVrijeTekstContainer = new WebMarkupContainer("clientPortaalVrijeTekstContainer");
			clientPortaalVrijeTekstContainer.setOutputMarkupId(true);
			clientPortaalVrijeTekstContainer.setVisible(isSoOfCe);
			ComponentHelper.addTextArea(clientPortaalVrijeTekstContainer, "clientPortaalVrijeTekst", true, 255, inzien);
			add(clientPortaalVrijeTekstContainer);

			ScreenitDropdown<Gebruiker> gemachtigden = new ScreenitDropdown<Gebruiker>("gemachtigde",
				new SimpleListHibernateModel<>(instellingService.getActieveGebruikers(getModelObject())), new IChoiceRenderer<Gebruiker>()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Object getDisplayValue(Gebruiker object)
					{
						return object.getNaamVolledig();
					}

					@Override
					public String getIdValue(Gebruiker object, int index)
					{
						return object.getId().toString();
					}

					@Override
					public Gebruiker getObject(String id, IModel<? extends List<? extends Gebruiker>> choices)
					{
						if (id != null)
						{
							return choices.getObject().stream().filter(g -> g.getId().toString().equals(id)).findFirst().orElse(null);
						}
						return null;
					}
				});
			gemachtigden.setNullValid(true);
			gemachtigden.setEnabled(!inzien);
			gemachtigden.setVisible(!isCe);
			add(gemachtigden);

			add(new WebMarkupContainer("telNum1Label").setVisible(isSoOfCe));
			add(new WebMarkupContainer("telNum2LabelSo").setVisible(isSo));
			add(new WebMarkupContainer("telNum2LabelCe").setVisible(isCe));
			add(new Label("emailLabel", new StringResourceModel("mail.extra-label." + organisatieType.name())).setVisible(isCe));

			ComponentHelper.addTextField(this, "uziAbonneenummer", true, 8, inzien)
				.add(new UniqueFieldValidator<Instelling, String>(Instelling.class, instelling.getId(), "uziAbonneenummer", hibernateService)).add(new PatternValidator("[0-9]{8}"))
				.setVisible(organisatieType == OrganisatieType.ZORGINSTELLING);

			FormComponent<String> rootOid = new TextField<String>("rootOid");
			rootOid.add(new PatternValidator(Constants.OID_EXTENSION_PATTERN));
			rootOid.setEnabled(!inzien);
			rootOid.setRequired(true);
			rootOid.add(new UniqueFieldValidator<Instelling, String>(Instelling.class, instelling.getId(), "rootOid", hibernateService));
			rootOid.setVisible(
				organisatieType == OrganisatieType.COLOSCOPIELOCATIE || organisatieType == OrganisatieType.PA_LABORATORIUM || organisatieType == OrganisatieType.BMHK_LABORATORIUM);
			add(rootOid);
		}

		private void addAnnulerenButton()
		{
			AjaxLink<Gebruiker> annuleren = new AjaxLink<Gebruiker>("annuleren")
			{

				private static final long serialVersionUID = 1L;

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
			AjaxLink<Instelling> inActiveren = new ConfirmingIndicatingAjaxLink<Instelling>("inActiveren", dialog, "question.remove.organisatie")
			{

				@Override
				public void onClick(AjaxRequestTarget target)
				{
					Instelling organisatie = (Instelling) HibernateHelper.deproxy(OrganisatieEditForm.this.getModelObject());
					String feedbackMessageId = "";
					if (organisatie.getActief())
					{
						if (organisatie instanceof BeoordelingsEenheid)
						{
							BeoordelingsEenheid beoordelingsEenheid = (BeoordelingsEenheid) organisatie;
							feedbackMessageId = beoordelingsEenheidService.magWordenGeinactiveerd(beoordelingsEenheid);
						}
						else if (organisatie instanceof CentraleEenheid)
						{
							CentraleEenheid centraleEenheid = (CentraleEenheid) organisatie;
							feedbackMessageId = centraleEenheidService.magWordenGeinactiveerd(centraleEenheid);
						}
					}
					else
					{
						if (organisatie instanceof BeoordelingsEenheid)
						{
							BeoordelingsEenheid beoordelingsEenheid = (BeoordelingsEenheid) organisatie;
							feedbackMessageId = beoordelingsEenheidService.magWordenGeactiveerd(beoordelingsEenheid);
						}
					}

					if (StringUtils.isNotBlank(feedbackMessageId))
					{
						error(getString(feedbackMessageId));
						return;
					}

					organisatie.setActief(Boolean.FALSE.equals(organisatie.getActief()));

					hibernateService.saveOrUpdate(organisatie);
					if (Boolean.FALSE.equals(organisatie.getActief()))
					{
						logAction(LogGebeurtenis.ORGANISATIE_INACTIVEERD, organisatie);
					}
					else
					{
						logAction(LogGebeurtenis.ORGANISATIE_ACTIVEERD, organisatie);
					}

					setResponsePage(OrganisatieZoeken.class);
				}

				@Override
				protected boolean skipConfirmation()
				{
					return Boolean.FALSE.equals(OrganisatieEditForm.this.getModelObject().getActief());
				}

			};

			if (Boolean.FALSE.equals(organisatie.getActief()))
			{
				inActiveren.add(new Label("inActiverenTitle", "Activeren"));
			}
			else
			{
				inActiveren.add(new Label("inActiverenTitle", "Inactiveren"));
			}
			inActiveren.setVisible(organisatie.getId() != null && isMinimumActie(actie, Actie.VERWIJDEREN) && !OrganisatieType.RIVM.equals(organisatie.getOrganisatieType()));
			add(inActiveren);
		}

		private void addOpslaanButton(Actie actie)
		{
			ScreenitIndicatingAjaxSubmitLink opslaan = new ScreenitIndicatingAjaxSubmitLink("opslaan", this)
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					Instelling organisatie = getModelObject();

					boolean nieuw = organisatie.getId() == null;
					instellingService.saveOrUpdate(organisatie);
					BasePage.markeerFormulierenOpgeslagen(target);
					if (nieuw)
					{
						logAction(LogGebeurtenis.ORGANISATIE_NIEUW, organisatie);
						setCurrentSelectedOrganisatie(organisatie);
						setResponsePage(new OrganisatieBasisgegevens(ModelUtil.cModel(organisatie)));
					}
					else
					{
						logAction(LogGebeurtenis.ORGANISATIE_WIJZIG, organisatie);
					}

					if (organisatie instanceof ColoscopieCentrum)
					{
						ColoscopieCentrum coloscopieCentrum = (ColoscopieCentrum) organisatie;
						if (coloscopieCentrum.getPostcodeCoordinaten() == null)
						{
							warn(getLocalizer().getString("action.save.organisatie", this));
							warn("Bij dit adres kunnen geen postcode coordinaten worden bepaald." + " De reisafstand voor de burger kan hierdoor niet worden berekend.");
						}
						else
						{
							info(getLocalizer().getString("action.save.organisatie", this));
						}
					}
					else
					{
						info(getLocalizer().getString("action.save.organisatie", this));
					}
				}
			};
			opslaan.setVisible(isMinimumActie(actie, Actie.AANPASSEN));
			add(opslaan);
		}

	}

}
