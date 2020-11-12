package nl.rivm.screenit.mamma.planning.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

@Configuration
@Order(Ordered.HIGHEST_PRECEDENCE)
@ImportResource(
	locations = {
		"classpath:applicationContext-jndi.xml",
		"classpath:applicationContext-jndi-planning-bk.xml",

		"classpath:applicationContext-mail.xml",

		"classpath:applicationContext-hibernate5-extension-spring.xml",
		"classpath:applicationContext-spring-extension-injection.xml",
		"classpath:applicationContext-autowired.xml",

		"classpath:applicationContext-hibernate.xml",
		"classpath:applicationContext-jms.xml",
		"classpath:applicationContext-preference-module.xml",
		"classpath:applicationContext-documentupload.xml",
	})
public class ImportXmlOrder
{
}
