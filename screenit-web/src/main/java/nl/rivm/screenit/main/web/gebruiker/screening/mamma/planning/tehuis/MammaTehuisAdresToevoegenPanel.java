package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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

import nl.rivm.screenit.main.service.mamma.MammaTehuisAdresService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.PatternValidator;

public abstract class MammaTehuisAdresToevoegenPanel extends GenericPanel<MammaTehuisAdres>
{

	@SpringBean
	private MammaTehuisAdresService tehuisAdresService;

	public MammaTehuisAdresToevoegenPanel(String id, IModel<MammaTehuisAdres> model)
	{
		super(id, model);

		Form<MammaTehuisAdres> locatieForm = new ScreenitForm<>("locatieForm", model);
		add(locatieForm);
		ComponentHelper.addTextField(locatieForm, "huisnummer", true, 10, Integer.class, false);
		ComponentHelper.addTextField(locatieForm, "huisletter", false, 1, String.class, false).add(new PatternValidator(".*[a-zA-Z].*"));
		ComponentHelper.addTextField(locatieForm, "huisnummerToevoeging", false, 26, String.class, false);

		ComponentHelper.newPostcodeTextField(locatieForm, "postcode", true, false);
		locatieForm.add(ComponentHelper.newCheckBox("locatieVanTehuis"));

		locatieForm.add(new ScreenitIndicatingAjaxSubmitLink("opslaan", locatieForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaTehuisAdres adres = locatieForm.getModelObject();
				boolean isAdresAlGekoppeld = tehuisAdresService.isAdresAlGekoppeld(adres);

				if (isAdresAlGekoppeld)
				{
					error(getString("error.adres.al.gekoppeld"));
				}
				else
				{
					tehuisAdresService.adresToevoegen(adres, ScreenitSession.get().getLoggedInInstellingGebruiker());
					onClickOpslaan(target);
				}

				boolean zijnErClienten = tehuisAdresService.countClienten(adres.getTehuis(), MammaTehuisSelectie.TEHUIS_ADRES, adres) > 0;
				if (!zijnErClienten)
				{
					ScreenitSession.get().warn(getString("warn.geen.clienten.op.adres"));
				}
			}
		});
	}

	abstract void onClickOpslaan(AjaxRequestTarget target);

}
