
package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.WachtwoordConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
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
import org.apache.wicket.markup.html.form.CheckBox;
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
	level = ToegangLevel.REGIO,
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON })
public class WachtwoordConfiguratiePage extends ParameterisatieBasePage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private ParameterisatieService parameterisatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	public WachtwoordConfiguratiePage()
	{
		add(new WachtwoordConfiguratieForm("form", new CompoundPropertyModel<>(new Model<>(parameterisatieService.loadWachtwoordConfiguratie()))));
	}

	private class WachtwoordConfiguratieForm extends Form<WachtwoordConfiguratie>
	{

		private static final long serialVersionUID = 1L;

		private ToegangLevel level;

		private boolean inzien;

		public WachtwoordConfiguratieForm(String id, IModel<WachtwoordConfiguratie> model)
		{
			super(id, model);
			level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			Actie actie = autorisatieService.getActieVoorMedewerker(ScreenitSession.get().getLoggedInInstellingGebruiker(), ScreenitSession.get().getCurrentSelectedMedewerker(),
				Recht.GEBRUIKER_BEHEER_PARAMETERISATIE);
			inzien = !isMinimumActie(actie, Actie.AANPASSEN);

			final TextArea<String> tekst = new TextArea<String>("wachtwoordemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "wachtwoordaanvragenEmail";
				}
			};
			tekst.setOutputMarkupId(true);
			tekst.setEnabled(!inzien);
			add(tekst);
			add(ComponentHelper.addTextField(this, "wachtwoordemailsubject", true, 2000, inzien));
			add(ComponentHelper.addTextField(this, "dagenWachtwoordGeldig", true, 20, Integer.class, inzien));
			add(ComponentHelper.addTextField(this, "maximumFoutieveAanmeldpogingen", true, 20, Integer.class, inzien));
			add(ComponentHelper.addTextField(this, "foutieveAanmeldpogingenTimeout", true, 20, Integer.class, inzien));

			add(new CheckBox("wachtwoordaanvragen").setEnabled(!inzien));

			add(ComponentHelper.addTextField(this, "uziemailsubject", true, 2000, inzien));
			final TextArea<String> uziTekst = new TextArea<String>("uziemail")
			{

				private static final long serialVersionUID = 1L;

				@Override
				public String getMarkupId()
				{
					return "uziaanvragenEmail";
				}

			};
			uziTekst.setOutputMarkupId(true);
			uziTekst.setEnabled(!inzien);
			add(uziTekst);

			AjaxSubmitLink opslaan = new AjaxSubmitLink("opslaan")
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					BasePage.markeerFormulierenOpgeslagen(target);
					parameterisatieService.saveOrUpdateWachtwoordConfiguratie(getModelObject());
					logService.logGebeurtenis(LogGebeurtenis.PARAMETERISATIE_WIJZIG, ScreenitSession.get().getLoggedInAccount(), "Wachtwoord configuratie is aangepast.");
					info("Wachtwoord configuratie is opgeslagen.");
				}

			};
			opslaan.setEnabled(!inzien);
			opslaan.setVisible(!inzien && ToegangLevel.LANDELIJK.equals(level));
			add(opslaan);
		}
	}
}
