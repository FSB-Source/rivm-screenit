package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange;

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

import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaScreeningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import java.util.ArrayList;
import java.util.List;

@SecurityConstraint(
	constraint = ShiroConstraint.HasPermission,
	checkScope = true,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
	recht = { Recht.GEBRUIKER_MAMMA_EXCHANGE_UPLOAD },
	organisatieTypeScopes = { OrganisatieType.RADIOLOGIEAFDELING, OrganisatieType.MAMMAPOLI, OrganisatieType.ZORGINSTELLING, OrganisatieType.RIVM })
public class MammaExchangeUploadPage extends MammaScreeningBasePage
{
	@SpringBean
	private MammaUploadBeeldenService uploadBeeldenService;

	private WebMarkupContainer openstaandeUploadVerzoekenContainer;

	private Panel formulierPanel;

	public MammaExchangeUploadPage()
	{
		formulierPanel = (Panel) new EmptyPanel("formulier").setOutputMarkupId(true).setOutputMarkupPlaceholderTag(true).setVisible(false);
		add(formulierPanel);
		maakWerklijst();
	}

	private void maakFormulier(AjaxRequestTarget target, MammaUploadBeeldenVerzoek uploadBeeldenVerzoek)
	{
		formulierPanel = new MammaExchangeUploadPanel("formulier", ModelUtil.cModel(uploadBeeldenVerzoek))
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				formulierPanel.setVisible(false);
				maakWerklijst();
				target.add(formulierPanel, openstaandeUploadVerzoekenContainer);
			}
		};
		formulierPanel.setOutputMarkupId(true);
		formulierPanel.setOutputMarkupPlaceholderTag(true);

		addOrReplace(formulierPanel);
		target.add(formulierPanel);
	}

	private void maakWerklijst()
	{
		openstaandeUploadVerzoekenContainer = new WebMarkupContainer("openstaandeUploadVerzoekenContainer");
		openstaandeUploadVerzoekenContainer.setOutputMarkupId(true);
		openstaandeUploadVerzoekenContainer.setOutputMarkupPlaceholderTag(true);
		addOrReplace(openstaandeUploadVerzoekenContainer);

		List<IColumn<MammaUploadBeeldenVerzoek, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<>(Model.of("Bsn"), "screeningRonde.dossier.client.persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("screeningRonde.dossier.client.persoon"));
		columns.add(new EnumPropertyColumn<>(Model.of("Datum verzoek"), "creatieDatum", "creatieDatum"));
		columns.add(new EnumPropertyColumn<>(Model.of("Status"), "status", "status"));
		columns.add(new PropertyColumn<>(Model.of("Melding"), "laatsteUploadPoging.statusMelding"));

		MammaExchangeUploadVerzoekDataProvider dataProvider = new MammaExchangeUploadVerzoekDataProvider(ModelUtil.sModel(ScreenitSession.get().getInstelling()));
		ScreenitDataTable<MammaUploadBeeldenVerzoek, String> openstaandeUploadVerzoekenTabel = new ScreenitDataTable<MammaUploadBeeldenVerzoek, String>(
			"openstaandeUploadVerzoekenTabel", columns, dataProvider, 10, Model.of("verzoek(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaUploadBeeldenVerzoek> model)
			{
				super.onClick(target, model);
				maakFormulier(target, model.getObject());
				openstaandeUploadVerzoekenContainer.setVisible(false);
				target.add(openstaandeUploadVerzoekenContainer);
			}
		};
		openstaandeUploadVerzoekenContainer.addOrReplace(openstaandeUploadVerzoekenTabel);

	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return MammaExchangeBasePage.getContextMenuItemsList();
	}
}
