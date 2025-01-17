package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.util.Arrays;

import nl.rivm.screenit.model.MammaDagEnDagdeelFilter;
import nl.rivm.screenit.model.enums.BeschikbareAfspraakDagen;
import nl.rivm.screenit.model.enums.Dagdeel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Check;
import org.apache.wicket.markup.html.form.CheckGroup;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;

public abstract class DagEnDagdeelFilterPanel extends GenericPanel<MammaDagEnDagdeelFilter>
{
	protected DagEnDagdeelFilterPanel(String id, MammaDagEnDagdeelFilter dagEnDagdeelFilter)
	{
		super(id, new CompoundPropertyModel<>(dagEnDagdeelFilter));
		var keuzeDagSelector = new CheckGroup<>("dagen");
		keuzeDagSelector.add(new ListView<>("dagKeuzeList", Arrays.asList(BeschikbareAfspraakDagen.values()))
		{
			@Override
			protected void populateItem(ListItem<BeschikbareAfspraakDagen> dag)
			{
				Check<BeschikbareAfspraakDagen> check = new Check<>("checkboxDag", dag.getModel());
				dag.add(check);
				dag.add(new EnumLabel<>("labelDag", dag.getModelObject()));
			}
		});
		keuzeDagSelector.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onCheckBoxChange(target);
			}
		});

		var keuzeDagdelenSelector = new CheckGroup<>("dagdelen");
		keuzeDagdelenSelector.add(new ListView<>("dagdeelKeuzeList", Arrays.asList(Dagdeel.values()))
		{
			@Override
			protected void populateItem(ListItem<Dagdeel> dagdeel)
			{
				Check<Dagdeel> check = new Check<>("checkboxDagdeel", dagdeel.getModel());
				dagdeel.add(check);
				dagdeel.add(new EnumLabel<>("labelDagdeel", dagdeel.getModelObject()));
			}
		});
		keuzeDagdelenSelector.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				onCheckBoxChange(target);
			}
		});

		add(keuzeDagSelector);
		add(keuzeDagdelenSelector);
	}

	protected abstract void onCheckBoxChange(AjaxRequestTarget target);
}
