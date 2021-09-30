package nl.rivm.screenit.main.web.gebruiker.testen.preferences;

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

import java.util.Arrays;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.MailVerzenden;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.NumberTextField;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import com.google.common.base.Strings;

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

	private final Model<MailVerzenden> mailVerzendenModel;

	private final Model<String> alternatiefAdresModel;

	private Model<Boolean> bkKansberekeningEnabledModel;

	private Model<Integer> bkKansberekeningDefaultOpkomstKansModel;

	public TestPreferencesPage()
	{
		Form<Void> form = new ScreenitForm<>("form");
		add(form);

		MailVerzenden mailVerzendenPreference = preferenceService.getEnum(PreferenceKey.MAIL_VERZENDEN.toString(), MailVerzenden.class);
		if (mailVerzendenPreference == null)
		{
			mailVerzendenPreference = MailVerzenden.AAN;
		}
		mailVerzendenModel = new Model<>(mailVerzendenPreference);

		alternatiefAdresModel = new Model<>(preferenceService.getString(PreferenceKey.ALTERNATIEF_ADRES.toString()));

		RadioChoice<MailVerzenden> mailVerzenden = new RadioChoice<>("mailVerzenden", mailVerzendenModel, Arrays.asList(MailVerzenden.values()),
			new EnumChoiceRenderer<>());
		mailVerzenden.setPrefix("<label class=\"radio\">");
		mailVerzenden.setSuffix("</label>");
		form.add(mailVerzenden);

		final TextField<String> alternatiefAdres = new TextField<>("alternatiefAdres", alternatiefAdresModel);
		alternatiefAdres.setEnabled(MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendenModel.getObject()));
		alternatiefAdres.setOutputMarkupId(true);
		form.add(alternatiefAdres);

		mailVerzenden.add(new AjaxFormSubmitBehavior("change")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				alternatiefAdres.setEnabled(MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendenModel.getObject()));
				target.add(alternatiefAdres);
			}
		});

		kansBerekeningBKFields(form);

		BootstrapDialog dialog = new BootstrapDialog("dialog");
		add(dialog);

		form.add(new ConfirmingIndicatingAjaxSubmitLink<Void>("opslaan", form, dialog, "opslaan.mailing.aan")
		{
			@Override
			protected boolean skipConfirmation()
			{
				return !MailVerzenden.AAN.equals(mailVerzendenModel.getObject());
			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				if (MailVerzenden.ALTERNATIEF_ADRES.equals(mailVerzendenModel.getObject())
					&& Strings.isNullOrEmpty(alternatiefAdresModel.getObject()))
				{
					error("Alternatief adres verplicht");
				}
				else
				{
					preferenceService.putEnum(PreferenceKey.MAIL_VERZENDEN.name(), mailVerzendenModel.getObject());
					preferenceService.putString(PreferenceKey.ALTERNATIEF_ADRES.toString(), alternatiefAdresModel.getObject());
					success("Voorkeuren opgeslagen");
				}

				opslaanBkKansberekening();
			}
		});
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
