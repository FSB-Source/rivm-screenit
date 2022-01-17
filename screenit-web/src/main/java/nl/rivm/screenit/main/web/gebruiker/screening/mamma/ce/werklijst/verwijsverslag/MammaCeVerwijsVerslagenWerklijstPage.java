package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.verwijsverslag;

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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeZoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_VERWIJSVERSLAGEN_CONTROLLEREN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeVerwijsVerslagenWerklijstPage extends AbstractMammaCeWerklijst
{
	@SpringBean
	private MammaBeoordelingService beoordelingsService;

	public MammaCeVerwijsVerslagenWerklijstPage()
	{
		super();
		createResultTable();
		MammaCeZoekPanel zoekPanel = new MammaCeZoekPanel("zoekContainer", zoekObjectModel, this, resultatenContainer)
		{

			@Override
			protected List<MammaBeoordelingStatus> getRemoveFromDefaultFilter()
			{
				List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
				beoordelingStatussen.add(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
				return beoordelingStatussen;
			}

			@Override
			protected List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen()
			{
				List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
				beoordelingStatussen.add(MammaBeoordelingStatus.VERSLAG_GEREED);
				beoordelingStatussen.add(MammaBeoordelingStatus.VERSLAG_GOEDKEURING_OPGESCHORT);
				beoordelingStatussen.add(MammaBeoordelingStatus.UITSLAG_ONGUNSTIG);
				beoordelingStatussen.add(MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING);
				return beoordelingStatussen;
			}
		};
		add(zoekPanel);
	}

	private void createResultTable()
	{
		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(getOnderzoeksdatumColumn());
		columns.add(getVerslagdatumColumn());
		columns.add(getClientColumn());
		columns.add(getGeboortedatumColumn());
		columns.add(getBsnColumn());
		columns.add(getSeColumn());
		columns.add(getBeColumn());
		columns.add(getStatusColumn());

		resultatenContainer.add(new ScreenitDataTable<MammaBeoordeling, String>("resultaten", columns, onderzoekDataProvider, 10, null)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> beoordelingModel)
			{
				MammaBeoordeling beoordeling = beoordelingModel.getObject();
				if (beoordeling.getStatus() == MammaBeoordelingStatus.UITSLAG_ONGUNSTIG)
				{
					File file = beoordelingsService.genereerPdfVoorOngunstigeUitslagBrief(beoordeling);
					dialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, file)
					{
						@Override
						protected void onClose(AjaxRequestTarget target)
						{
							super.onClose(target);
							setResponsePage(new MammaCeVerwijsVerslagenWerklijstPage());
						}
					});
				}
				else
				{
					setResponsePage(new MammaCeVerwijsVerslagPage(ModelUtil.cModel(beoordeling)));
				}
			}
		});
	}

}
