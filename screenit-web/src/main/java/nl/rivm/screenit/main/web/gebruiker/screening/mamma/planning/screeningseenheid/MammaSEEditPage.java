package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitBooleanDropdown;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.Ip4Validator;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDuurMinderValideAfspraak;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.timefield.TimeField;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_SE_BEHEER },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaSEEditPage extends MammaPlanningBasePage
{
	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private HibernateService hibernateService;

	private BootstrapDialog dialog;

	private final boolean magSeAanpassen;

	private Label regioLabel;

	private Panel mammografentabel;

	private Form<MammaScreeningsEenheid> seWijzigenForm;

	private ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheid;

	private ScreenitDropdown<BeoordelingsEenheid> tijdelijkeBeoordelingsEenheid;

	@SpringBean
	private InstellingService instellingService;

	MammaSEEditPage(IModel<MammaScreeningsEenheid> model)
	{
		setDefaultModel(model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		magSeAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_SE_BEHEER, Actie.AANPASSEN) && !ingelogdNamensRegio;

		seWijzigenForm = new ScreenitForm<>("seWijzigenForm", model);
		createOrReplaceCodeComponent(seWijzigenForm, null);
		createOrReplaceNaamComponent(seWijzigenForm, null);
		createOrReplaceIpAdresComponent(seWijzigenForm, null);

		regioLabel = maakRegioLabel();
		seWijzigenForm.add(regioLabel);

		final MammaScreeningsEenheid screeningsEenheid = model.getObject();
		beoordelingsEenheid = maakBeoordelingenDropdown(screeningsEenheid.getBeoordelingsEenheid());
		tijdelijkeBeoordelingsEenheid = maakTijdelijkeBeoordelingenDropdown(screeningsEenheid.getBeoordelingsEenheid());
		seWijzigenForm.add(beoordelingsEenheid);
		seWijzigenForm.add(tijdelijkeBeoordelingsEenheid);

		DatePicker tijdelijkeBeVanafDatum = ComponentHelper.newDatePicker("tijdelijkeBeVanafDatum", new PropertyModel<>(getDefaultModel(), "tijdelijkeBeVanafDatum"));
		seWijzigenForm.add(tijdelijkeBeVanafDatum);

		DatePicker tijdelijkeBeTotEnMetDatum = ComponentHelper.newDatePicker("tijdelijkeBeTotEnMetDatum", new PropertyModel<>(getDefaultModel(), "tijdelijkeBeTotEnMetDatum"));
		seWijzigenForm.add(tijdelijkeBeTotEnMetDatum);

		ScreenitBooleanDropdown heeftLiftDropdownChoice = new ScreenitBooleanDropdown("heeftLift", true, !magAanpassen);
		seWijzigenForm.add(heeftLiftDropdownChoice);

		seWijzigenForm.add(new ScreenitBooleanDropdown("tomosyntheseMogelijk", true, magSeAanpassen));

		ScreenitDropdown duurMinderValideAfspraak = new ScreenitDropdown<>("duurMinderValideAfspraak", Arrays.asList(MammaDuurMinderValideAfspraak.values()),
			new EnumChoiceRenderer<>(this));
		duurMinderValideAfspraak.setRequired(true);
		seWijzigenForm.add(duurMinderValideAfspraak);

		MinderValideAfspraakPeriodeTimeField minderValidePeriode1Vanaf = new MinderValideAfspraakPeriodeTimeField("minderValidePeriode1Vanaf");
		MinderValideAfspraakPeriodeTimeField minderValidePeriode1TotEnMet = new MinderValideAfspraakPeriodeTimeField("minderValidePeriode1TotEnMet");
		seWijzigenForm.add(minderValidePeriode1Vanaf);
		seWijzigenForm.add(minderValidePeriode1TotEnMet);

		MinderValideAfspraakPeriodeTimeField minderValidePeriode2Vanaf = new MinderValideAfspraakPeriodeTimeField("minderValidePeriode2Vanaf");
		MinderValideAfspraakPeriodeTimeField minderValidePeriode2TotEnMet = new MinderValideAfspraakPeriodeTimeField("minderValidePeriode2TotEnMet");
		seWijzigenForm.add(minderValidePeriode2Vanaf);
		seWijzigenForm.add(minderValidePeriode2TotEnMet);

		ScreenitBooleanDropdown isMobielDropdownChoice = new ScreenitBooleanDropdown("isMobiel", "Mobiel", "Vast", true, !magAanpassen);
		seWijzigenForm.add(isMobielDropdownChoice);
		seWijzigenForm.add(new TijdelijkeBeAanSeValidator(this, (s) -> getString((String) s),
			tijdelijkeBeoordelingsEenheid, beoordelingsEenheid, tijdelijkeBeVanafDatum, tijdelijkeBeTotEnMetDatum));
		seWijzigenForm.add(new IndicatingAjaxButton("opslaan")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String meldingMinderValidePeriodes = screeningsEenheidService.valideerMinderValideAfspraakPeriodes(getScreeningsEenheid());
				if (StringUtils.isNotBlank(meldingMinderValidePeriodes))
				{
					error(getString(meldingMinderValidePeriodes));
					return;
				}

				boolean seProxyEnMammograafIpsMatchen = screeningsEenheidService.ipAdressenHebbenZelfdeGemeenschappelijkeBlokken(getScreeningsEenheid());
				if (!seProxyEnMammograafIpsMatchen)
				{
					error("De eerste drie blokken van de IP-adressen mammograaf en server screeningseenheid moeten overeen komen.");
					return;
				}
				getScreeningsEenheid().getMammografen().forEach(mammograaf -> hibernateService.reload(mammograaf));
				boolean succes = screeningsEenheidService.saveOrUpdateSE(getScreeningsEenheid(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (succes)
				{
					success(getString("message.gegevensopgeslagen"));
					createOrReplaceCodeComponent(seWijzigenForm, target);
					createOrReplaceNaamComponent(seWijzigenForm, target);
					createOrReplaceIpAdresComponent(seWijzigenForm, null);
					BasePage.markeerFormulierenOpgeslagen(target);
					mammografentabel.setVisible(true);
					target.add(mammografentabel);
				}
			}

		}.setVisible(magAanpassen || magSeAanpassen));
		addInActiverenButton(seWijzigenForm);

		add(seWijzigenForm);

		add(new IndicatingAjaxLink<Void>("terug")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaSEZoekenPage.class);
			}
		});

		mammografentabel = new MammaMammografenPanel("mammografenPanel", ModelUtil.csModel(screeningsEenheid), magAanpassen);
		mammografentabel.setOutputMarkupId(true);
		mammografentabel.setOutputMarkupPlaceholderTag(true);
		mammografentabel.setVisible(screeningsEenheid.getId() != null); 

		add(mammografentabel);
	}

	private void createOrReplaceCodeComponent(Form<MammaScreeningsEenheid> seWijzigenForm, AjaxRequestTarget target)
	{
		ComponentHelper.addTextField(seWijzigenForm, "code", true, 255, String.class, !magAanpassen || seWijzigenForm.getModelObject().getCode() != null)
			.add(new UniqueFieldValidator<>(MammaScreeningsEenheid.class, getScreeningsEenheid().getId(), "code", hibernateService, true))
			.add(new MammaSECodeValidator());

		if (target != null)
		{
			target.add(seWijzigenForm);
		}
	}

	private void createOrReplaceNaamComponent(Form<MammaScreeningsEenheid> seWijzigenForm, AjaxRequestTarget target)
	{
		ComponentHelper.addTextField(seWijzigenForm, "naam", true, 255, String.class, !magAanpassen);

		if (target != null)
		{
			target.add(seWijzigenForm);
		}
	}

	private void createOrReplaceIpAdresComponent(Form<MammaScreeningsEenheid> seWijzigenForm, AjaxRequestTarget target)
	{
		ComponentHelper.addTextField(seWijzigenForm, "ipAdres", true, 255, String.class, !magAanpassen)
			.add(new UniqueFieldValidator<>(MammaScreeningsEenheid.class, getScreeningsEenheid().getId(), "ipAdres", hibernateService, true))
			.add(Ip4Validator.getInstance());

		if (target != null)
		{
			target.add(seWijzigenForm);
		}
	}

	private Label maakRegioLabel()
	{
		Label label = new Label("beoordelingsEenheid.parent.regio.naam");
		label.setVisible(!ingelogdNamensRegio);
		label.setOutputMarkupId(true);
		return label;
	}

	private ScreenitDropdown<BeoordelingsEenheid> maakBeoordelingenDropdown(BeoordelingsEenheid huidigeBeoordelingsEenheid)
	{
		List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden(huidigeBeoordelingsEenheid);

		ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheidDropdown = new ScreenitDropdown<>("beoordelingsEenheid",
			ModelUtil.listRModel(mogelijkeBeoordelingsEenheden, false), new ChoiceRenderer<>("naam"));

		beoordelingsEenheidDropdown.setRequired(true);
		beoordelingsEenheidDropdown.setEnabled(magAanpassen || magSeAanpassen);
		beoordelingsEenheidDropdown.setOutputMarkupId(true);

		beoordelingDropdown(beoordelingsEenheidDropdown);
		return beoordelingsEenheidDropdown;
	}

	private ScreenitDropdown<BeoordelingsEenheid> maakTijdelijkeBeoordelingenDropdown(BeoordelingsEenheid huidigeBeoordelingsEenheid)
	{
		List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden(huidigeBeoordelingsEenheid);
		mogelijkeBeoordelingsEenheden.removeIf(be -> be.equals(huidigeBeoordelingsEenheid));

		ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheidDropdown = new ScreenitDropdown<>("tijdelijkeBeoordelingsEenheid",
			ModelUtil.listRModel(mogelijkeBeoordelingsEenheden, false), new ChoiceRenderer<>("naam"));

		beoordelingsEenheidDropdown.setRequired(false);
		beoordelingsEenheidDropdown.setEnabled(magAanpassen || magSeAanpassen);
		beoordelingsEenheidDropdown.setNullValid(true);
		beoordelingsEenheidDropdown.setOutputMarkupId(true);

		beoordelingDropdown(beoordelingsEenheidDropdown);
		return beoordelingsEenheidDropdown;
	}

	private void beoordelingDropdown(ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheidDropdown)
	{
		if (!ingelogdNamensRegio)
		{
			beoordelingsEenheidDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					target.add(regioLabel);
				}
			});
		}
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden(BeoordelingsEenheid huidigeBeoordelingsEenheid)
	{
		List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden;
		if (magSeAanpassen)
		{
			mogelijkeBeoordelingsEenheden = instellingService.getActieveInstellingen(BeoordelingsEenheid.class);
		}
		else
		{
			ScreeningOrganisatie regio = ScreenitSession.get().getScreeningOrganisatie();
			mogelijkeBeoordelingsEenheden = instellingService.getActieveBeoordelingseenhedenBinnenRegio(regio);
		}

		if (huidigeBeoordelingsEenheid != null && !mogelijkeBeoordelingsEenheden.contains(huidigeBeoordelingsEenheid))
		{
			mogelijkeBeoordelingsEenheden.add(huidigeBeoordelingsEenheid);
		}

		return mogelijkeBeoordelingsEenheden;
	}

	private void addInActiverenButton(Form<MammaScreeningsEenheid> seWijzigenForm)
	{
		AjaxLink<Gebruiker> inActiveren = new ConfirmingIndicatingAjaxLink<>("inActiveren", dialog, "question.remove.se")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaScreeningsEenheid screeningsEenheid = getScreeningsEenheid();
				String feedbackMessageId;
				if (!screeningsEenheid.getActief())
				{
					feedbackMessageId = screeningsEenheidService.magWordenGeactiveerd(screeningsEenheid);

				}
				else
				{
					feedbackMessageId = screeningsEenheidService.magWordenGeinactiveerd(screeningsEenheid);
				}

				if (StringUtils.isNotBlank(feedbackMessageId))
				{
					error(getString(feedbackMessageId));
					return;
				}

				screeningsEenheid.setActief(Boolean.FALSE.equals(screeningsEenheid.getActief()));
				screeningsEenheidService.saveOrUpdateSE(screeningsEenheid, ScreenitSession.get().getLoggedInInstellingGebruiker());
				setResponsePage(MammaSEZoekenPage.class);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return Boolean.FALSE.equals(getScreeningsEenheid().getActief());
			}

		};

		MammaScreeningsEenheid screeningsEenheid = getScreeningsEenheid();

		if (Boolean.FALSE.equals(screeningsEenheid.getActief()))
		{
			inActiveren.add(new Label("inActiverenTitle", "Activeren"));
		}
		else
		{
			inActiveren.add(new Label("inActiverenTitle", "Inactiveren"));
		}
		boolean magInActiveren = screeningsEenheid.getId() != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.VERWIJDEREN)
			&& ingelogdNamensRegio;
		inActiveren.setVisible(magInActiveren);
		seWijzigenForm.add(inActiveren);
	}

	private MammaScreeningsEenheid getScreeningsEenheid()
	{
		return (MammaScreeningsEenheid) getDefaultModelObject();
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaSEZoekenPage.class;
	}

	public ScreenitDropdown<BeoordelingsEenheid> getBeoordelingsEenheid()
	{
		return beoordelingsEenheid;
	}

	public ScreenitDropdown<BeoordelingsEenheid> getTijdelijkeBeoordelingsEenheid()
	{
		return tijdelijkeBeoordelingsEenheid;
	}

	private static class MinderValideAfspraakPeriodeTimeField extends TimeField
	{
		MinderValideAfspraakPeriodeTimeField(String id)
		{
			super(id, true);
		}

		@Override
		public void convertInput()
		{
			super.convertInput();
			if (getInput().startsWith(":"))
			{
				setDate(null);
			}
			else
			{
				setDate(DateUtil.zetSeconden(getDate(), 0));
			}
			setConvertedInput(getDate());
		}
	}
}
