package nl.rivm.screenit.main.web.gebruiker.testen.preferences;

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

import java.util.Arrays;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.validator.ScreenitTelefoonnummerValidator;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.SmsVerzenden;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.NumberTextField;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.jetbrains.annotations.NotNull;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class TestPreferencesPage extends TestenBasePage
{
	@SpringBean
	private SimplePreferenceService preferenceService;

	private Model<MailVerzenden> mailVerzendenModel;

	private Model<SmsVerzenden> smsVerzendenModel;

	private Model<String> alternatiefAdresModel;

	private Model<String> alternatiefMobielNummerModel;

	private Model<Boolean> bkKansberekeningEnabledModel;

	private Model<Integer> bkKansberekeningDefaultOpkomstKansModel;

	private WebMarkupContainer alternatiefMobielnummerContainer;

	private RadioChoice<SmsVerzenden> smsVerzendenRadioChoice;

	private WebMarkupContainer alternatiefAdresContainer;

	private RadioChoice<MailVerzenden> mailVerzendenRadioChoice;

	public TestPreferencesPage()
	{
		Form<Void> form = new ScreenitForm<>("form");
		add(form);

		alternatiefMailAdres(form);

		alternatiefMobielNummer(form);

		kansBerekeningBKFields(form);

		formOpslaan(form);
	}

	private void alternatiefMobielNummer(Form<Void> form)
	{
		maakAlternatiefMobielNummerContainer();
		maakSmsVerzendenRadioChoiceKnop();

		form.add(alternatiefMobielnummerContainer);
		form.add(smsVerzendenRadioChoice);

		alternatiefMobielnummerContainer.setVisible(SmsVerzenden.ALTERNATIEF_MOBIELNUMMER.equals(smsVerzendenModel.getObject()));

		smsVerzendenAjaxOnChange();
	}

	@NotNull
	private void maakAlternatiefMobielNummerContainer()
	{
		alternatiefMobielnummerContainer = new WebMarkupContainer("alternatiefMobielnummerContainer");
		alternatiefMobielnummerContainer.setOutputMarkupPlaceholderTag(true);

		alternatiefMobielNummerModel = new Model<>(preferenceService.getString(PreferenceKey.ALTERNATIEF_MOBIELNUMMER.toString()));
		var alternatiefMobielnummerTextfield = new TextField<>("alternatiefMobielnummer", alternatiefMobielNummerModel);
		alternatiefMobielnummerTextfield.setRequired(true);
		alternatiefMobielnummerTextfield.setOutputMarkupPlaceholderTag(true);
		alternatiefMobielnummerTextfield.add(ScreenitTelefoonnummerValidator.mobielNederlandsNummer());
		alternatiefMobielnummerTextfield.add(StringValidator.maximumLength(GbaPersoon.MAX_PHONE_LENGTH));

		alternatiefMobielnummerContainer.add(alternatiefMobielnummerTextfield);

	}

	private void smsVerzendenAjaxOnChange()
	{
		smsVerzendenRadioChoice.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				alternatiefMobielnummerContainer.setVisible(smsVerzendenModel.getObject().equals(SmsVerzenden.ALTERNATIEF_MOBIELNUMMER));
				ajaxRequestTarget.add(alternatiefMobielnummerContainer);
			}
		});
	}

	@NotNull
	private void maakSmsVerzendenRadioChoiceKnop()
	{
		SmsVerzenden smsVerzendenPreference = preferenceService.getEnum(PreferenceKey.SMS_VERZENDEN.name(), SmsVerzenden.class);
		smsVerzendenModel = new Model<>(smsVerzendenPreference);

		var smsVerzendenOpties = Arrays.asList(SmsVerzenden.ALTERNATIEF_MOBIELNUMMER, SmsVerzenden.UIT);

		smsVerzendenRadioChoice = new RadioChoice<>("smsVerzenden", smsVerzendenModel, smsVerzendenOpties,
			new EnumChoiceRenderer<>(this));
		smsVerzendenRadioChoice.setPrefix("<label class=\"radio\">");
		smsVerzendenRadioChoice.setSuffix("</label>");
		smsVerzendenRadioChoice.setRequired(true);
	}

	private void formOpslaan(Form<Void> form)
	{
		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				preferenceEmailOpslaan();
				preferenceSmsOpslaan();
				opslaanBkKansberekening();
				success("Voorkeuren opgeslagen");

			}

			private void preferenceSmsOpslaan()
			{
				preferenceService.putEnum(PreferenceKey.SMS_VERZENDEN.name(), smsVerzendenModel.getObject());
				preferenceService.putString(PreferenceKey.ALTERNATIEF_MOBIELNUMMER.toString(), alternatiefMobielNummerModel.getObject());
			}

			private void preferenceEmailOpslaan()
			{
				preferenceService.putEnum(PreferenceKey.MAIL_VERZENDEN.name(), mailVerzendenModel.getObject());
				preferenceService.putString(PreferenceKey.ALTERNATIEF_ADRES.toString(), alternatiefAdresModel.getObject());
			}
		});
	}

	private void alternatiefMailAdres(Form<Void> form)
	{
		MailVerzenden mailVerzendenPreference = preferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.name(), MailVerzenden.class);
		mailVerzendenModel = new Model<>(mailVerzendenPreference);
		alternatiefAdresModel = new Model<>(preferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.toString()));

		maakMailVerzendenRadioChoiceKnop();
		maakAlternatiefAdresTextField();

		form.add(mailVerzendenRadioChoice);
		form.add(alternatiefAdresContainer);

		alternatiefAdresContainer.setVisible(MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendenModel.getObject()));

		mailVerzendenAjaxOnChange();
	}

	private void mailVerzendenAjaxOnChange()
	{
		mailVerzendenRadioChoice.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget ajaxRequestTarget)
			{
				alternatiefAdresContainer.setVisible(MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendenModel.getObject()) && mailVerzendenModel.getObject() != null);
				ajaxRequestTarget.add(alternatiefAdresContainer);
			}
		});
	}

	@NotNull
	private void maakAlternatiefAdresTextField()
	{
		alternatiefAdresContainer = new WebMarkupContainer("alternatiefAdresContainer");
		alternatiefAdresContainer.setVisible(true);
		alternatiefAdresContainer.setOutputMarkupPlaceholderTag(true);

		final TextField<String> alternatiefAdres = new TextField<>("alternatiefAdres", alternatiefAdresModel);
		alternatiefAdres.setRequired(true);
		alternatiefAdresContainer.setOutputMarkupPlaceholderTag(true);
		alternatiefAdresContainer.add(alternatiefAdres);

	}

	private void maakMailVerzendenRadioChoiceKnop()
	{
		var mailVerzendenOpties = Arrays.asList(MailVerzenden.ALTERNATIEF_ADRES, MailVerzenden.UIT);
		mailVerzendenRadioChoice = new RadioChoice<>("mailVerzenden", mailVerzendenModel, mailVerzendenOpties,
			new EnumChoiceRenderer<>(this));
		mailVerzendenRadioChoice.setPrefix("<label class=\"radio\">");
		mailVerzendenRadioChoice.setSuffix("</label>");
		mailVerzendenRadioChoice.setRequired(true);
	}

	private void kansBerekeningBKFields(Form<Void> form)
	{
		bkKansberekeningEnabledModel = Model.of(preferenceService.getBoolean(PreferenceKey.KANSBEREKENING_BK.toString()));
		form.add(ComponentHelper.newCheckBox("kansberekeningEnabled", bkKansberekeningEnabledModel));

		bkKansberekeningDefaultOpkomstKansModel = Model.of(preferenceService.getInteger(PreferenceKey.KANSBEREKENING_BK_TEST_DEFAULT_OPKOMSTKANS.toString(), 50));
		form.add(new NumberTextField<Integer>("defaultOpkomstkans", bkKansberekeningDefaultOpkomstKansModel).setMinimum(0).setMaximum(100));
	}

	private void opslaanBkKansberekening()
	{
		preferenceService.putBoolean(PreferenceKey.KANSBEREKENING_BK.toString(), bkKansberekeningEnabledModel.getObject());
		preferenceService.putInteger(PreferenceKey.KANSBEREKENING_BK_TEST_DEFAULT_OPKOMSTKANS.toString(), bkKansberekeningDefaultOpkomstKansModel.getObject());
	}
}
