package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public abstract class CervixUitnodigingenPanel extends Panel
{

	private Panel uitnodigingenPanel;

	public CervixUitnodigingenPanel(String id)
	{
		super(id);
		uitnodigingenPanel = new EmptyPanel("uitnodigingenPanel");
		uitnodigingenPanel.setOutputMarkupPlaceholderTag(true);
		add(uitnodigingenPanel);
	}

	void replaceUitnodigingenPanel(AjaxRequestTarget target, List<CervixUitnodiging> uitnodigingen)
	{
		ScreenitSession.get().setZoekObject(this.getClass(), Model.of(uitnodigingen));

		CervixUitnodigingenProvider uitnodigingenProvider = new CervixUitnodigingenProvider(uitnodigingen);
		List<IColumn<CervixUitnodiging, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Monster-id"), "monster.monsterId"));
		columns.add(new EnumPropertyColumn<>(Model.of("Type monster"), "monsterType"));
		columns.add(new AbstractColumn<CervixUitnodiging, String>(Model.of("Verzenddatum"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<CervixUitnodiging>> cellItem, String componentId, IModel<CervixUitnodiging> uitnodigingModel)
			{
				CervixUitnodiging uitnodiging = uitnodigingModel.getObject();
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					if (uitnodiging.getBrief().getMergedBrieven() != null)
					{
						cellItem.add(new Label(componentId, uitnodiging.getBrief().getMergedBrieven().getPrintDatum()));
					}
					else
					{
						cellItem.add(new Label(componentId, ""));
					}
					break;
				case ZAS:
					CervixZas zas = (CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster());
					cellItem.add(new Label(componentId, zas.getVerstuurd()));
					break;
				}
			}
		});
		columns.add(new AbstractColumn<CervixUitnodiging, String>(Model.of("Status monster"))
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void populateItem(Item<ICellPopulator<CervixUitnodiging>> item, String id, IModel<CervixUitnodiging> uitnodigingModel)
			{
				CervixUitnodiging uitnodiging = uitnodigingModel.getObject();
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					CervixUitstrijkje uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(uitnodiging.getMonster());
					item.add(new EnumLabel<>(id, uitstrijkje.getUitstrijkjeStatus()));
					break;
				case ZAS:
					CervixZas zas = (CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster());
					item.add(new EnumLabel<>(id, zas.getZasStatus()));
					break;
				}
			}
		});
		columns.add(new ClientColumn<>("screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("screeningRonde.dossier.client.persoon"));

		ScreenitDataTable<CervixUitnodiging, String> newUitnodigingenPanel = new ScreenitDataTable<CervixUitnodiging, String>("uitnodigingenPanel", columns, uitnodigingenProvider,
			100, null, false)
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target, IModel<CervixUitnodiging> uitnodigingModel)
			{
				CervixUitnodigingenPanel.this.setUitnodiging(target, uitnodigingModel.getObject());
			}
		};
		newUitnodigingenPanel.setOutputMarkupId(true);
		uitnodigingenPanel.replaceWith(newUitnodigingenPanel);
		uitnodigingenPanel = newUitnodigingenPanel;
		target.add(uitnodigingenPanel);
	}

	abstract protected void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging);

}
