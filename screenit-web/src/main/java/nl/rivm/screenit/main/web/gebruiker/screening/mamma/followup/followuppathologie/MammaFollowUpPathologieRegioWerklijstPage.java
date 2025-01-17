package nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.followuppathologie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaFollowUpInstellingDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.followup.AbstractMammaFollowUpPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_FOLLOW_UP_PATHOLOGIE_WERKLIJST },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE })
public class MammaFollowUpPathologieRegioWerklijstPage extends AbstractMammaFollowUpPage
{

	@SpringBean
	private HibernateService hibernateService;

	private WebMarkupContainer refreshContainer;

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		refreshContainer = new WebMarkupContainer("refreshContainer");
		refreshContainer.setOutputMarkupId(Boolean.TRUE);
		add(refreshContainer);

		MammaFollowUpPathologieRegioProvider followUpPathologieRegioProvider = new MammaFollowUpPathologieRegioProvider(
			ModelUtil.sModel(ScreenitSession.get().getScreeningOrganisatie()));

		List<IColumn<MammaFollowUpInstellingDto, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Naam"), "instellingNaam", "instellingNaam"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon 1"), "telefoon"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon 2"), "telefoon2"));
		columns.add(new PropertyColumn<>(Model.of("Gebeld op"), "laatstGebeld", "laatstGebeld"));
		ScreenitDataTable<MammaFollowUpInstellingDto, String> table = new ScreenitDataTable<MammaFollowUpInstellingDto, String>("resultaten", columns,
			followUpPathologieRegioProvider,
			10, Model.of("instelling(en)"))
		{

			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaFollowUpInstellingDto> model)
			{
				super.onClick(target, model);
				Instelling instelling = hibernateService.get(Instelling.class, model.getObject().getInstellingId());
				setResponsePage(new MammaFollowUpPathologieWerklijstPage(ModelUtil.sModel(instelling)));
			}
		};
		refreshContainer.add(table);
	}
}
