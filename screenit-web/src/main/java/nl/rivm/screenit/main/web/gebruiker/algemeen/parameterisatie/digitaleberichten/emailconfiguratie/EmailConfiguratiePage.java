
package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.digitaleberichten.emailconfiguratie;

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

import nl.rivm.screenit.main.model.EmailConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.ParameterisatieBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.LogService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	level = ToegangLevel.LANDELIJK,
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class EmailConfiguratiePage extends ParameterisatieBasePage
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	public EmailConfiguratiePage()
	{
		add(new EmailConfiguratieForm("form", new CompoundPropertyModel<>(new Model<>(parameterisatieService.loadEmailConfiguratie()))));
		add(new EmailTemplatesPanel("emailTemplateConfiguratie"));
	}

	private class EmailConfiguratieForm extends Form<EmailConfiguratie>
	{

		private static final long serialVersionUID = 1L;

		private ToegangLevel level;

		private boolean inzien;

		public EmailConfiguratieForm(String id, IModel<EmailConfiguratie> model)
		{
			super(id, model);
			level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			Actie actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
				Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			inzien = !isMinimumActie(actie, Actie.AANPASSEN);

			final TextArea<String> inactiverenemail = new TextArea<String>("inactiverenemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "inactiverenemail";
				}
			};

			inactiverenemail.setRequired(true);
			inactiverenemail.setOutputMarkupId(true);
			inactiverenemail.setEnabled(!inzien);
			add(inactiverenemail);
			add(ComponentHelper.addTextField(this, "inactiverenemailsubject", true, 2000, inzien));
			final TextArea<String> geblokkeerdemail = new TextArea<String>("geblokkeerdemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "geblokkeerdemail";
				}
			};
			geblokkeerdemail.setRequired(true);
			geblokkeerdemail.setOutputMarkupId(true);
			geblokkeerdemail.setEnabled(!inzien);
			add(geblokkeerdemail);
			add(ComponentHelper.addTextField(this, "geblokkeerdemailsubject", true, 2000, inzien));

			final TextArea<String> registrerenHuisartsmail = new TextArea<String>("registrerenhuisartsemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "registrerenhuisartsemail";
				}
			};
			registrerenHuisartsmail.setRequired(true);
			registrerenHuisartsmail.setOutputMarkupId(true);
			registrerenHuisartsmail.setEnabled(!inzien);
			add(registrerenHuisartsmail);
			add(ComponentHelper.addTextField(this, "registrerenhuisartsemailsubject", true, 2000, inzien));

			final TextArea<String> wachtwoordHuisartsmail = new TextArea<String>("wachtwoordhuisartsemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "wachtwoordhuisartsemail";
				}
			};
			wachtwoordHuisartsmail.setRequired(true);
			wachtwoordHuisartsmail.setOutputMarkupId(true);
			wachtwoordHuisartsmail.setEnabled(!inzien);
			add(wachtwoordHuisartsmail);
			add(ComponentHelper.addTextField(this, "wachtwoordhuisartsemailsubject", true, 2000, inzien));

			AjaxSubmitLink opslaan = new AjaxSubmitLink("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					BasePage.markeerFormulierenOpgeslagen(target);
					parameterisatieService.saveOrUpdateEmailConfiguratie(getModelObject());
					logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, ScreenitSession.get().getLoggedInAccount(), "E-mail configuratie aangepast",
						Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX);
					info(getString("menu.algemeen.parameterisatie.medewerkerconfiguratie") + " is opgeslagen.");
				}

			};
			opslaan.setEnabled(!inzien);
			opslaan.setVisible(!inzien && ToegangLevel.LANDELIJK.equals(level));
			add(opslaan);
		}
	}
}
