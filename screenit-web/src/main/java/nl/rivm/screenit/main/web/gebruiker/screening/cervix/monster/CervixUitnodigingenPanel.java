package nl.rivm.screenit.main.web.gebruiker.screening.cervix.monster;

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
import java.util.List;

import nl.rivm.screenit.main.service.cervix.CervixUitnodigingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.EnumPropertyColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

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
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CervixUitnodigingenPanel extends Panel
{

	@SpringBean
	private CervixUitnodigingService uitnodigingService;

	private Panel uitnodigingenPanel;

	protected CervixUitnodigingenPanel(String id)
	{
		super(id);
		uitnodigingenPanel = new EmptyPanel("uitnodigingenPanel");
		uitnodigingenPanel.setOutputMarkupPlaceholderTag(true);
		add(uitnodigingenPanel);
	}

	void replaceUitnodigingenPanel(AjaxRequestTarget target, List<CervixUitnodiging> uitnodigingen)
	{
		ScreenitSession.get().setZoekObject(this.getClass(), ModelUtil.listRModel(uitnodigingen));

		var uitnodigingenProvider = new CervixUitnodigingenProvider(uitnodigingen);
		var columns = new ArrayList<IColumn<CervixUitnodiging, String>>();
		columns.add(new PropertyColumn<>(Model.of("Monster-id"), "monster.monsterId"));
		columns.add(new EnumPropertyColumn<>(Model.of("Type monster"), "monsterType"));
		columns.add(new AbstractColumn<>(Model.of("Verzenddatum"))
		{

			@Override
			public void populateItem(Item<ICellPopulator<CervixUitnodiging>> cellItem, String componentId, IModel<CervixUitnodiging> uitnodigingModel)
			{
				var uitnodiging = uitnodigingModel.getObject();
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					var mergedBrieven = BriefUtil.getMergedBrieven(uitnodiging.getBrief());
					if (mergedBrieven != null)
					{
						cellItem.add(new Label(componentId, mergedBrieven.getPrintDatum()));
					}
					else
					{
						cellItem.add(new Label(componentId, ""));
					}
					break;
				case ZAS:
					var zas = (CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster());
					cellItem.add(new Label(componentId, zas.getVerstuurd()));
					break;
				}
			}
		});
		columns.add(new AbstractColumn<>(Model.of("Status monster"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<CervixUitnodiging>> item, String id, IModel<CervixUitnodiging> uitnodigingModel)
			{
				var uitnodiging = uitnodigingModel.getObject();
				switch (uitnodiging.getMonsterType())
				{
				case UITSTRIJKJE:
					var uitstrijkje = (CervixUitstrijkje) HibernateHelper.deproxy(uitnodiging.getMonster());
					item.add(new EnumLabel<>(id, uitstrijkje.getUitstrijkjeStatus()));
					break;
				case ZAS:
					var zas = (CervixZas) HibernateHelper.deproxy(uitnodiging.getMonster());
					item.add(new EnumLabel<>(id, zas.getZasStatus()));
					break;
				}
			}
		});
		columns.add(new ClientColumn<>("screeningRonde.dossier.client"));
		columns.add(new GeboortedatumColumn<>("screeningRonde.dossier.client.persoon"));

		var newUitnodigingenPanel = new ScreenitDataTable<>("uitnodigingenPanel", columns, uitnodigingenProvider,
			100, null, false)
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<CervixUitnodiging> uitnodigingModel)
			{
				var uitnodiging = uitnodigingModel.getObject();
				var ingelogdNamensOrganisatie = ScreenitSession.get().getInstelling();
				if (uitnodigingService.magMonsterVerwerktWordenDoorLab(ingelogdNamensOrganisatie, uitnodiging))
				{
					setUitnodiging(target, uitnodiging);
				}
				else
				{
					error(String.format(getString("zas.mag.niet.verwerkt.worden"), ingelogdNamensOrganisatie.getNaam()));
				}
			}
		};
		newUitnodigingenPanel.setOutputMarkupId(true);
		uitnodigingenPanel.replaceWith(newUitnodigingenPanel);
		uitnodigingenPanel = newUitnodigingenPanel;
		target.add(uitnodigingenPanel);
	}

	protected abstract void setUitnodiging(AjaxRequestTarget target, CervixUitnodiging uitnodiging);

}
