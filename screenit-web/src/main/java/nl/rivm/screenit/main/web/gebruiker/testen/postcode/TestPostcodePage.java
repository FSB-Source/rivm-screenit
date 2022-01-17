package nl.rivm.screenit.main.web.gebruiker.testen.postcode;

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

import java.math.BigDecimal;

import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.testen.TestenBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.TESTEN, bevolkingsonderzoekScopes = {
	Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA})
public class TestPostcodePage extends TestenBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<TestPostcode> postcodeModel;

	public TestPostcodePage()
	{
		TestPostcode testPostcode = new TestPostcode();
		postcodeModel = new CompoundPropertyModel<TestPostcode>(testPostcode);
		Form<TestPostcode> form = new ScreenitForm<TestPostcode>("form", postcodeModel);

		form.add(new TextField<String>("orgLatitude"));
		form.add(new TextField<String>("orgLongitude"));
		form.add(new TextField<String>("clientLatitude"));
		form.add(new TextField<String>("clientLongitude"));

		form.add(new AjaxButton("submitAfstand", form)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				TestPostcode testPostcode = (TestPostcode) form.getModelObject();
				double distance = BigDecimalUtil.berekenDistance(new BigDecimal(testPostcode.getOrgLatitude()), new BigDecimal(testPostcode.getOrgLongitude()),
					new BigDecimal(testPostcode.getClientLatitude()), new BigDecimal(testPostcode.getClientLongitude()));
				info("afstand berekend: " + distance + " km");
			}
		});

		add(form);
	}

	@Override
	protected void detachModel()
	{
		super.detachModel();
		ModelUtil.nullSafeDetach(postcodeModel);
	}
}
