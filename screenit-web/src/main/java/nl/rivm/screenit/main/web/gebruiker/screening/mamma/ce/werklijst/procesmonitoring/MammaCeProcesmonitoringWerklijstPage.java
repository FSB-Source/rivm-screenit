package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.procesmonitoring;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.MammaCeZoekPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.popups.MammaCeKoppelRadioloogAanBeoordelingPopup;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.AbstractMammaCeWerklijst;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.verwijsverslag.MammaCeVerwijsVerslagPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_PROCESMONITORING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeProcesmonitoringWerklijstPage extends AbstractMammaCeWerklijst
{
	private final MammaCeProcesmonitoringDataProvider dataProvider;

	private final BootstrapDialog dialog;

	public MammaCeProcesmonitoringWerklijstPage()
	{
		super();
		dataProvider = new MammaCeProcesmonitoringDataProvider(MammaOnderzoek_.CREATIE_DATUM, zoekObjectModel);
		dialog = new BootstrapDialog("dialog");
		add(dialog);
		createResultTable();
		MammaCeZoekPanel zoekPanel = new MammaCeZoekPanel("zoekContainer", zoekObjectModel, this, resultatenContainer)
		{

			@Override
			protected List<MammaBeoordelingStatus> getRemoveFromDefaultFilter()
			{
				return new ArrayList<>();
			}

			@Override
			protected List<MammaBeoordelingStatus> getMammaMogelijkeBeoordelingFilterStatussen()
			{
				return new ArrayList<>(
					Arrays.asList(MammaBeoordelingStatus.EERSTE_LEZING, MammaBeoordelingStatus.TWEEDE_LEZING, MammaBeoordelingStatus.DISCREPANTIE, MammaBeoordelingStatus.ARBITRAGE,
						MammaBeoordelingStatus.VERSLAG_MAKEN, MammaBeoordelingStatus.VERSLAG_AFGEKEURD, MammaBeoordelingStatus.VERSLAG_GOEDKEURING_OPGESCHORT));
			}
		};
		add(zoekPanel);
	}

	private void createResultTable()
	{
		List<IColumn<MammaBeoordeling, String>> columns = new ArrayList<>();
		columns.add(getOnderzoeksdatumColumn());
		columns.add(getClientColumn());
		columns.add(getGeboortedatumColumn());
		columns.add(getBsnColumn());
		columns.add(getSeColumn());
		columns.add(getBeColumn());
		columns.add(getStatusColumn());
		columns.add(getTypeOnderzoekColumn());

		resultatenContainer.add(new ScreenitDataTable<>("resultaten", columns, dataProvider, 10, Model.of("beoordeling(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaBeoordeling> model)
			{
				super.onClick(target, model);
				MammaBeoordeling beoordeling = model.getObject();
				switch (beoordeling.getStatus())
				{
				case VERSLAG_MAKEN:
				case VERSLAG_AFGEKEURD:
					toewijzenRadioloogPopup(target, model);
					break;
				case VERSLAG_GOEDKEURING_OPGESCHORT:
					gaNaarCeVerwijsPagina(model);
					break;
				default:
					break;
				}
			}
		});
	}

	private void gaNaarCeVerwijsPagina(IModel<MammaBeoordeling> model)
	{
		setResponsePage(new MammaCeVerwijsVerslagPage(ModelUtil.ccModel(model.getObject())));
	}

	private void toewijzenRadioloogPopup(AjaxRequestTarget target, IModel<MammaBeoordeling> beoordeling)
	{
		dialog.openWith(target, new MammaCeKoppelRadioloogAanBeoordelingPopup(IDialog.CONTENT_ID, beoordeling)
		{
			@Override
			public void close(AjaxRequestTarget target)
			{
				dialog.close(target);
			}
		});
	}

}
