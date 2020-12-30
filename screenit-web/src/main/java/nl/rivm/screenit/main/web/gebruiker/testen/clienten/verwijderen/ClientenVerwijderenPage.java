package nl.rivm.screenit.main.web.gebruiker.testen.clienten.verwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.ClientenVerwijderenTestService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.TESTEN,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON,
		Bevolkingsonderzoek.CERVIX,
		Bevolkingsonderzoek.MAMMA
	})
public class ClientenVerwijderenPage extends TestenBasePage
{
	@SpringBean
	private ClientenVerwijderenTestService clientenVerwijderenTestService;

	private IModel<String> bsns = new Model<>("");

	public ClientenVerwijderenPage()
	{

		ScreenitForm<Object> form = new ScreenitForm<>("form");
		add(form);

		form.add(new TextArea<>("bsns", bsns));
		form.add(new AjaxSubmitLink("formatten")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				bsns.setObject(bsns.getObject().replaceAll("[\\t\\n\\r]+", ", "));
			}
		});
		form.add(new IndicatingAjaxButton("personenBsnVerwijderen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String message = clientenVerwijderenTestService.clientenVerwijderen(bsns.getObject());
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
	}
}
