package nl.rivm.screenit.main.web.gebruiker.screening.mamma.palga;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.mamma.MammaPalgaService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_PALGA_CSV_UITWISSELING },
	organisatieTypeScopes = { OrganisatieType.RIVM })
public class MammaPalgaUitwisselingPage extends GebruikerBasePage
{
	@SpringBean
	private MammaPalgaService palgaService;

	public MammaPalgaUitwisselingPage()
	{
		add(new MammaPalgaImportPanel("importPanel"));
		IModel<UploadDocument> export = ModelUtil.cModel(palgaService.getExport());
		add(export != null ? new MammaPalgaExportPanel("exportPanel", export) : new EmptyPanel("exportPanel"));
	}

	@Override
	protected GebruikerHoofdMenuItem getActieveMenuItem()
	{
		return GebruikerHoofdMenuItem.MAMMA;
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return new ArrayList<>(Arrays.asList(
			new GebruikerMenuItem("label.tab.mammascreening.palga.uitwisseling", MammaPalgaUitwisselingPage.class)));
	}
}
