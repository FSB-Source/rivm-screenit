package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitDateTextField;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.validator.AchternaamValidator;
import nl.rivm.screenit.main.web.component.validator.TussenvoegselValidator;
import nl.rivm.screenit.main.web.component.validator.VoorlettersValidator;
import nl.rivm.screenit.main.web.component.validator.VoornaamValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadGebruikerImageFormComponent;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.UploadGebruikerImageType;
import nl.rivm.screenit.main.web.gebruiker.login.PasswordChangePanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Titel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.GebruikersService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.StamtabellenService;
import nl.rivm.screenit.service.WachtwoordService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.yubikey.model.YubiKey;

import org.apache.commons.lang.StringUtils;
import org.apache.shiro.crypto.hash.Sha512Hash;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.OnChangeAjaxBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.form.DateTextField;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.PasswordTextField;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import org.apache.wicket.validation.validator.PatternValidator;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MEDEWERKER_BEHEER,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class MedewerkerBasisgegevens extends MedewerkerBeheer
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private MedewerkerService medewerkerService;

	@SpringBean
	private GebruikersService gebruikersService;

	@SpringBean
	private WachtwoordService wachtwoordService;

	@SpringBean
	private StamtabellenService stamtabellenService;

	@SpringBean
	private AuthenticatieService authenticatieService;

	private Label bigNummerLabel;

	private Component bigNummer;

	private WebMarkupContainer yubiContainer;

	private WebMarkupContainer uziContainer;

	private WebMarkupContainer geenWachtwoordContainer;

	private WebMarkupContainer laatsteWachtwoordWijzigingContainer;

	public MedewerkerBasisgegevens()
	{
		init(ModelUtil.ccModel(getCurrentSelectedMedewerker()));
	}

	public MedewerkerBasisgegevens(IModel<Gebruiker> model)
	{
		init(model);
	}

	private void init(IModel<Gebruiker> model)
	{
		Gebruiker medewerker = model.getObject();

		if (medewerker.getYubiKey() == null)
		{
			medewerker.setYubiKey(new YubiKey());
		}

		Label label = new Label("label", Model.of("Bewerk medewerker"));
		if (medewerker.getId() == null)
		{
			label = new Label("label", Model.of("Medewerker toevoegen"));
			medewerker.setMedewerkercode(gebruikersService.getNextMedewerkercode());
		}
		add(label);
		if (model.getObject().getId() != null)
		{
			add(new MedewerkerPaspoortPanel("paspoort", model));
		}
		else
		{
			add(new WebMarkupContainer("paspoort").setVisible(false));
		}

		add(new MedewerkerEditForm("medewerkerForm", model));
	}

	private void logAction(LogGebeurtenis gebeurtenis, Gebruiker medewerker)
	{
		logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), "Medewerker: " + medewerker.getNaamVolledig());
	}

	public class MedewerkerEditForm extends ScreenitForm<Gebruiker>
	{

		@Getter
		@Setter
		private String wachtwoordControle;

		@Getter
		@Setter
		private String wachtwoord;

		private Boolean geblokkeerd = null;

		private boolean isBestaande;

		private boolean eigenGegevens;

		private boolean inzien;

		private boolean isBeheerder;

		private boolean isZorgverlener;

		private String privateYubiIdentity;

		private boolean bestaandUziNummer;

		private UploadGebruikerImageFormComponent handtekeningField;

		public MedewerkerEditForm(String id, IModel<Gebruiker> model)
		{
			super(id, model);
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			Gebruiker medewerker = getModelObject();
			List<Adres> adressen = medewerker.getAdressen();
			if (adressen == null)
			{
				medewerker.setAdressen(new ArrayList<>());
				adressen = medewerker.getAdressen();
			}
			if (adressen.isEmpty())
			{
				adressen.add(new Adres());
			}
			Actie actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), getModelObject(), Recht.GEBRUIKER_MEDEWERKER_BEHEER);

			isBestaande = medewerker.getId() != null;
			inzien = !isMinimumActie(actie, Actie.AANPASSEN);
			isBeheerder = isMinimumActie(actie, Actie.VERWIJDEREN);
			isZorgverlener = medewerker.getZorgverlener() != null && medewerker.getZorgverlener() == Boolean.TRUE;
			eigenGegevens = medewerker.equals(ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker()) && isMinimumActie(actie, Actie.AANPASSEN);
			bestaandUziNummer = medewerker.getUzinummer() != null;

			addOpslaanButton();
			addVerwijderenButton(medewerker);
			addAnnulerenButton();

			Map<String, Object> restrictions = new HashMap<>();
			restrictions.put("actief", Boolean.TRUE);

			ComponentHelper.addTextField(this, "achternaam", true, 100, inzien).add(new AchternaamValidator()).setLabel(Model.of("Achternaam"));

			ComponentHelper.addDropDownChoice(this, "aanhef", false, Aanhef.aanhefVormenMedewerkers(), inzien);

			ScreenitDropdown<Titel> titel = ComponentHelper.addDropDownChoiceINaam(this, "titel", false, ModelUtil.listRModel(stamtabellenService.getTitels(medewerker), false),
				inzien);
			titel.setNullValid(true);
			ScreenitDropdown<Functie> functie = ComponentHelper.addDropDownChoiceINaam(this, "functie", false,
				ModelUtil.listRModel(stamtabellenService.getFuncties(medewerker), false), inzien);
			functie.setNullValid(true);
			ComponentHelper.addTextField(this, "voornaam", false, 50, inzien).add(new VoornaamValidator());
			ComponentHelper.addTextField(this, "voorletters", false, 20, inzien).add(new VoorlettersValidator());
			ComponentHelper.addTextField(this, "tussenvoegsel", false, 20, inzien).add(new TussenvoegselValidator());
			handtekeningField = new UploadGebruikerImageFormComponent("handtekening", getModel(), UploadGebruikerImageType.HANDTEKENING);
			add(handtekeningField.setEnabled(!inzien));
			Component ondertekenaar = new TextArea<>("ondertekenaar").add(StringValidator.maximumLength(255));
			ondertekenaar.setEnabled(!inzien);
			add(ondertekenaar);

			bigNummerLabel = new Label("bigNummerLabel", "BIG nr.");
			bigNummerLabel.setVisible(isZorgverlener);
			bigNummerLabel.setOutputMarkupPlaceholderTag(true);
			add(bigNummerLabel);

			bigNummer = ComponentHelper.addTextField(this, "bignummer", false, 11, inzien)
				.add(new UniqueFieldValidator<>(Gebruiker.class, medewerker.getId(), "bignummer", hibernateService)).setVisible(isZorgverlener);
			bigNummer.setOutputMarkupPlaceholderTag(true);
			DateTextField geboortedatum = new ScreenitDateTextField("geboortedatum");
			geboortedatum.setOutputMarkupId(true);
			geboortedatum.setEnabled(!inzien);
			add(geboortedatum);
			uziContainer = new WebMarkupContainer("uzi-container");
			uziContainer.setOutputMarkupPlaceholderTag(true);
			ComponentHelper.addTextField(uziContainer, "uzinummer", false, 9, inzien)
				.add(new UniqueFieldValidator<>(Gebruiker.class, medewerker.getId(), "uzinummer", hibernateService)).add(new PatternValidator("[0-9]*"));
			add(uziContainer);

			ComponentHelper.addTextField(this, "patholoogId", false, 25, inzien).setEnabled(!inzien);

			Component zorgverlenerCheckbox = new CheckBox("zorgverlener").add(new OnChangeAjaxBehavior()
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					if (Boolean.TRUE.equals(getModelObject().getZorgverlener()))
					{
						isZorgverlener = Boolean.TRUE;
					}
					else
					{
						isZorgverlener = Boolean.FALSE;

					}
					bigNummer.setVisible(isZorgverlener);
					bigNummerLabel.setVisible(isZorgverlener);
					target.add(bigNummerLabel, bigNummer);

				}

			});
			zorgverlenerCheckbox.setEnabled(!inzien && isBeheerder);
			zorgverlenerCheckbox.setOutputMarkupId(true);
			add(zorgverlenerCheckbox);

			ComponentHelper.addTextField(this, "telefoonnummerwerk", false, 25, inzien);
			ComponentHelper.addTextField(this, "telefoonnummerextra", false, 25, inzien);

			ComponentHelper.addTextField(this, "emailextra", false, 255, inzien).add(EmailAddressValidator.getInstance()).setLabel(Model.of("E-mailadres"))
				.add(new UniqueFieldValidator<>(Gebruiker.class, medewerker.getId(), "emailextra", hibernateService, restrictions));
			ComponentHelper.addTextField(this, "adressen[0].plaats", false, 80, inzien);
			ComponentHelper.addTextField(this, "telefoonnummerprive", false, 25, inzien);

			yubiContainer = new WebMarkupContainer("yubi-container");
			yubiContainer.add(new PasswordTextField("privateYubiIdentity", new PropertyModel<>(MedewerkerEditForm.this, "privateYubiIdentity")).setRequired(false)
				.setOutputMarkupId(true).setEnabled(!inzien && isBeheerder).add(StringValidator.maximumLength(18)));
			ComponentHelper.addTextField(yubiContainer, "yubiKey.secretKey", false, 48, inzien || !isBeheerder);
			yubiContainer.setOutputMarkupPlaceholderTag(true);
			add(yubiContainer);
			addWachtwoordFragment(medewerker);
		}

		private void addAnnulerenButton()
		{
			AjaxLink<Gebruiker> annuleren = new AjaxLink<>("annuleren")
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

		private void addWachtwoordFragment(Gebruiker medewerker)
		{
			FormComponent<String> gebruikersnaam = ComponentHelper.addTextField(this, "gebruikersnaam", true, 30, inzien);
			gebruikersnaam.setEnabled(!inzien && isBeheerder);
			gebruikersnaam.add(new UniqueFieldValidator<>(Gebruiker.class, medewerker.getId(), "gebruikersnaam", hibernateService, true));
			gebruikersnaam.setLabel(Model.of("Gebruikersnaam"));

			geenWachtwoordContainer = new WebMarkupContainer("geenWachtwoord");
			geenWachtwoordContainer.setOutputMarkupPlaceholderTag(true);
			setZichtbaarheidGeenWachtwoordTekst(medewerker.getWachtwoord(), medewerker.getInlogMethode());

			add(geenWachtwoordContainer);

			ScreenitDropdown<InlogMethode> inlogMethode = ComponentHelper.newDropDownChoice("inlogMethode", new ListModel<>(Arrays.asList(InlogMethode.values())),
				new EnumChoiceRenderer<>(), true);

			inlogMethode.add(new OnChangeAjaxBehavior()
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					var gekozenInlogmethode = getModelObject().getInlogMethode();
					switch (gekozenInlogmethode)
					{
					case YUBIKEY:
						yubiContainer.setVisible(true);
						uziContainer.setVisible(false);
						break;
					case GEBRUIKERSNAAM_WACHTWOORD:
						yubiContainer.setVisible(false);
						uziContainer.setVisible(false);
						break;
					case UZIPAS:
						yubiContainer.setVisible(false);
						uziContainer.setVisible(true);
						break;
					default:
						break;
					}
					setZichtbaarheidGeenWachtwoordTekst(getModelObject().getWachtwoord(), gekozenInlogmethode);
					setZichtbaarheidLaatsteWachtwoordWijziging(gekozenInlogmethode);
					target.add(yubiContainer, uziContainer, geenWachtwoordContainer, laatsteWachtwoordWijzigingContainer);

				}

			});
			inlogMethode.setEnabled(!inzien && isBeheerder);
			WebMarkupContainer inlogMethodeContainer = new WebMarkupContainer("inlogMethodeContainer");
			inlogMethodeContainer.setOutputMarkupId(true);
			add(inlogMethodeContainer);
			inlogMethodeContainer.add(inlogMethode);

			ComponentHelper.addTextField(this, "medewerkercode", false, 30, true);

			final WebMarkupContainer blokkeerContainer = new WebMarkupContainer("blokkeerContainer");
			blokkeerContainer.setOutputMarkupPlaceholderTag(true);

			geblokkeerd = !InlogStatus.OK.equals(medewerker.getInlogstatus());
			final Model<Boolean> geblokkeerdModel = Model.of(geblokkeerd);
			Component blokkeer = new CheckBox("disabledOfGeblokkeerd", geblokkeerdModel);
			blokkeer.setEnabled(!inzien);
			blokkeerContainer.setVisible(isBeheerder);

			blokkeer.add(new AjaxFormComponentUpdatingBehavior("click")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					geblokkeerd = geblokkeerdModel.getObject();
				}
			});

			blokkeerContainer.add(blokkeer);
			add(blokkeerContainer);

			add(ComponentHelper.newDatePicker("actiefVanaf").setEnabled(!inzien));
			add(ComponentHelper.newDatePicker("actiefTotEnMet").setEnabled(!inzien));

			laatsteWachtwoordWijzigingContainer = new WebMarkupContainer("laatsteWachtwoordWijzigingContainer");
			var laatsteWachtwoordWijziging = new TextField<>("laatsteWachtwoordWijziging", Model.of(DateUtil.formatShortDateTime(medewerker.getLaatsteKeerWachtwoordGewijzigd())));
			laatsteWachtwoordWijziging.setEnabled(false);

			laatsteWachtwoordWijzigingContainer.add(laatsteWachtwoordWijziging);
			laatsteWachtwoordWijzigingContainer.setOutputMarkupPlaceholderTag(true);
			setZichtbaarheidLaatsteWachtwoordWijziging(medewerker.getInlogMethode());

			add(laatsteWachtwoordWijzigingContainer);

			IndicatingAjaxLink<Object> resetWachtwoord = new IndicatingAjaxLink<>("resetWachtwoord")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{

					Gebruiker medewerker = MedewerkerEditForm.this.getModelObject();
					if (StringUtils.isNotBlank(medewerker.getEmailextra()))
					{
						medewerkerService.resetWachtwoord(medewerker);
						logService.logGebeurtenis(LogGebeurtenis.WACHTWOORD_GERESET, ScreenitSession.get().getLoggedInAccount(), "Medewerker: " + medewerker.getGebruikersnaam());
					}
					else
					{
						error(getString("error.geenmailadres.voor.reset.wachtwoord"));
					}
				}

			};
			resetWachtwoord.setVisible(isBestaande && !eigenGegevens && isBeheerder);
			add(resetWachtwoord);

			IndicatingAjaxLink<Object> changeWachtwoord = new IndicatingAjaxLink<>("changeWachtwoord")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					Gebruiker medewerker = MedewerkerEditForm.this.getModelObject();
					dialog.setContent(new PasswordChangePanel(IDialog.CONTENT_ID, medewerker)
					{
						@Override
						protected void onWachtwoordChanged(AjaxRequestTarget target, Gebruiker gebruiker)
						{
							dialog.close(target);
						}
					});
					dialog.open(target);
				}

			};
			changeWachtwoord.setVisible(isBestaande && eigenGegevens);
			add(changeWachtwoord);
		}

		private void setZichtbaarheidGeenWachtwoordTekst(String wachtwoord, InlogMethode inlogMethode)
		{
			var wachtwoordTextTonen = StringUtils.isBlank(wachtwoord) && InlogMethode.UZIPAS != inlogMethode;
			geenWachtwoordContainer.setVisible(wachtwoordTextTonen);
		}

		private void setZichtbaarheidLaatsteWachtwoordWijziging(InlogMethode inlogMethode)
		{
			laatsteWachtwoordWijzigingContainer.setVisible(InlogMethode.UZIPAS != inlogMethode);
		}

		private void addVerwijderenButton(Gebruiker medewerker)
		{
			AjaxLink<Gebruiker> inActiveren = new ConfirmingIndicatingAjaxLink<>("inActiveren", dialog, "question.remove.medewerker")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					Gebruiker medewerker = MedewerkerEditForm.this.getModelObject();

					medewerkerService.inActiveerGebruiker(medewerker);
					if (Boolean.FALSE.equals(medewerker.getActief()))
					{
						logAction(LogGebeurtenis.MEDEWERKER_INACTIVEERD, medewerker);
					}
					else
					{
						logAction(LogGebeurtenis.MEDEWERKER_ACTIVEERD, medewerker);
					}
					setResponsePage(MedewerkerZoeken.class);
				}

				@Override
				protected boolean skipConfirmation()
				{
					return Boolean.FALSE.equals(MedewerkerEditForm.this.getModelObject().getActief());
				}

			};

			if (Boolean.FALSE.equals(medewerker.getActief()))
			{
				inActiveren.add(new Label("inActiverenTitle", "Activeren"));
			}
			else
			{
				inActiveren.add(new Label("inActiverenTitle", "Inactiveren"));
			}
			inActiveren.setVisible(isBeheerder && medewerker.getId() != null);
			add(inActiveren);
		}

		private void addOpslaanButton()
		{
			ScreenitIndicatingAjaxSubmitLink opslaan = new ScreenitIndicatingAjaxSubmitLink("opslaan", this)
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					Gebruiker medewerker = getModelObject();
					if (medewerker.getInlogMethode().equals(InlogMethode.UZIPAS) || medewerker.getEmailextra() != null)
					{
						InlogStatus oldInlogstatus = medewerker.getInlogstatus();
						boolean wordGeblokkeerd = false;
						if (Boolean.TRUE.equals(geblokkeerd))
						{
							if (InlogStatus.OK.equals(oldInlogstatus))
							{
								wordGeblokkeerd = true;
								medewerker.setInlogstatus(InlogStatus.GEBLOKKEERD);
							}
						}
						else
						{
							if (InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(oldInlogstatus))
							{
								medewerker.setFoutieveInlogpogingen(0);
								medewerker.setTijdLaatsteFoutieveInlog(null);
							}
							medewerker.setInlogstatus(InlogStatus.OK);
						}

						YubiKey yubiKey = medewerker.getYubiKey();
						if (yubiKey != null && yubiKey.getSecretKey() != null)
						{
							yubiKey.setSecretKey(yubiKey.getSecretKey().replaceAll(" ", ""));
						}

						boolean error = false;
						if (eigenGegevens)
						{
							if (StringUtils.isBlank(wachtwoord))
							{
								if (!isBestaande)
								{
									error = true;
									error(getLocalizer().getString("error.medewerker.nopassword", this));
								}
							}
							else if (!wachtwoord.equals(wachtwoordControle))
							{
								error = true;
								error(getLocalizer().getString("error.password.notequals", this));
							}
						}

						if ((medewerker.getHandtekening() != null && medewerker.getHandtekening().getActief() || handtekeningField.hasFile()) != StringUtils
							.isNotBlank(medewerker.getOndertekenaar()))
						{
							error = true;
							error(getString("error.handtekening.velden.niet.goed.gevuld"));
						}

						if (!error)
						{
							if (isBestaande)
							{
								if (StringUtils.isNotBlank(wachtwoord))
								{
									wachtwoordService.setWachtwoord(medewerker, wachtwoord);
								}

								if (privateYubiIdentity != null)
								{
									yubiKey.setHashedSecretIdentity(new Sha512Hash(privateYubiIdentity.replaceAll(" ", ""), medewerker.getId().toString(),
										Constants.PASSWORDHASHINGITERATIONS).toHex());
									yubiKey.setSessionCounter(0);
									yubiKey.setUsageInSessionCounter(0);
								}

								logAction(LogGebeurtenis.MEDEWERKER_WIJZIG, medewerker);
								Date actiefTotEnMet = medewerker.getActiefTotEnMet();
								if (actiefTotEnMet != null)
								{ 
									medewerker.setActiefTotEnMet(DateUtil.eindDag(actiefTotEnMet));
								}
								medewerkerService.saveOrUpdateGebruiker(medewerker, isBestaande, wordGeblokkeerd);
								if (handtekeningField.hasFile())
								{
									handtekeningField.uploadImage(target);
								}
								setCurrentSelectedMedewerker(medewerker);

								if (medewerker.getInlogMethode().equals(InlogMethode.UZIPAS) && !bestaandUziNummer && medewerker.getUzinummer() != null)
								{
									authenticatieService.sendUziEmail(medewerker);
								}

								info(getLocalizer().getString("action.save.medewerker", this));
								BasePage.markeerFormulierenOpgeslagen(target);
							}
							else
							{
								if (medewerkerService.saveOrUpdateGebruiker(medewerker, isBestaande, wordGeblokkeerd))
								{
									if (handtekeningField.hasFile())
									{
										handtekeningField.uploadImage(target);
									}
									if (privateYubiIdentity != null)
									{
										yubiKey.setHashedSecretIdentity(new Sha512Hash(privateYubiIdentity.replaceAll(" ", ""), medewerker.getId().toString(),
											Constants.PASSWORDHASHINGITERATIONS).toHex());
										yubiKey.setSessionCounter(0);
										yubiKey.setUsageInSessionCounter(0);

										medewerkerService.saveOrUpdateGebruiker(medewerker, isBestaande, wordGeblokkeerd);
									}

									logAction(LogGebeurtenis.MEDEWERKER_NIEUW, medewerker);
									setCurrentSelectedMedewerker(medewerker);
									setResponsePage(new MedewerkerBasisgegevens(ModelUtil.cModel(medewerker)));

									ScreenitSession.get().info(getLocalizer().getString("action.save.medewerkernew", this));
									BasePage.markeerFormulierenOpgeslagen(target);
								}

							}
						}

					}
					else
					{
						ScreenitSession.get().error(getString("error.email.verplicht.geen.uzi"));
					}
				}
			};
			add(opslaan);
		}
	}
}
