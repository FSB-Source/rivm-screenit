
package nl.rivm.screenit.main.web.gebruiker.screening.cervix.houdbaarheid.zasreeks;

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

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidEditPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.cervix.CervixZasHoudbaarheid;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.PatternValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_ZAS_BATCH,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.CERVIX)
public class ZASReeksEditPage extends HoudbaarheidEditPage<CervixZasHoudbaarheid>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private HoudbaarheidService houdbaarheidService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public ZASReeksEditPage()
	{
		super(ModelUtil.cModel(new CervixZasHoudbaarheid()));
	}

	public ZASReeksEditPage(IModel<CervixZasHoudbaarheid> model)
	{
		super(model);
	}

	@Override
	protected IModel<String> getTitleModel(IModel<CervixZasHoudbaarheid> model)
	{
		final boolean isNieuw = model.getObject().getId() == null;
		return new IModel<String>()
		{
			private static final long serialVersionUID = 1L;

			@Override
			public String getObject()
			{
				if (isNieuw)
				{
					return "Nieuwe ZAS reeks";
				}
				return "Wijzigen ZAS reeks";
			}
		};
	}

	@Override
	protected void customizeBarcodeFields(TextField<String> barcodeStart, TextField<String> barcodeEnd)
	{
		PatternValidator validator = new PatternValidator("^Z{1}\\d{8}$");
		barcodeStart.add(validator);
		barcodeEnd.add(validator);
	}

	@Override
	protected Component createTypeField(String id)
	{
		return new WebMarkupContainer(id).setVisible(false);
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return ZASReeksOverzichtPage.class;
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.CERVIX;
	}
}
