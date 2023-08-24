
package nl.rivm.screenit.main.web.gebruiker.screening.colon.houdbaarheid.ifobtbatch;

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

import nl.rivm.screenit.main.service.HoudbaarheidService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.gedeeld.houdbaarheid.HoudbaarheidEditPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.IFOBTVervaldatum;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = false,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_SCREENING_IFOBT_BATCH,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class IFOBTBatchEditPage extends HoudbaarheidEditPage<IFOBTVervaldatum>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private HoudbaarheidService ifobtVervaldatumService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	public IFOBTBatchEditPage()
	{
		this(ModelUtil.cModel(new IFOBTVervaldatum()));
	}

	public IFOBTBatchEditPage(IModel<IFOBTVervaldatum> model)
	{
		super(model);

		if (model.getObject().getType() == null)
		{
			model.getObject().setType(IFOBTType.GOLD);
		}
	}

	@Override
	protected IModel<String> getTitleModel(IModel<IFOBTVervaldatum> model)
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
					return "Nieuwe FIT batch";
				}
				return "Wijzigen FIT batch";
			}
		};
	}

	@Override
	protected FormComponent<IFOBTType> createTypeField(String id)
	{
		ScreenitDropdown<IFOBTType> type = new ScreenitDropdown<IFOBTType>(id, Arrays.asList(IFOBTType.values()), new EnumChoiceRenderer<IFOBTType>());
		type.setRequired(true);
		return type;
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveSubMenuClass()
	{
		return IFOBTBatchOverzichtPage.class;
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.COLON;
	}
}
