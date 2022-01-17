package nl.rivm.screenit.mamma.se.proxy.services;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import nl.rivm.screenit.mamma.se.proxy.model.CacheProxyActie;
import nl.rivm.screenit.mamma.se.proxy.model.RequestTypeCentraal;

import org.springframework.http.HttpMethod;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;

public interface ProxyService
{

	void init();

	RequestEntity.BodyBuilder getProxyRequestEntity(String pathPostfix, HttpMethod method);

	<T> ResponseEntity sendCachableProxyRequest(RequestEntity requestEntity, Class<T> responseType, CacheProxyActie cacheActie);

	ResponseEntity<String> getRequest(RequestTypeCentraal requestType, CacheProxyActie cacheProxyActie);

	ResponseEntity<String> getPlanning(LocalDate datum, CacheProxyActie cacheProxyActie);

	void deleteOudePlanningCache();

	<T> ResponseEntity<T> sendUncheckedProxyRequest(RequestEntity requestEntity, Class<T> responseType);

	RequestEntity.BodyBuilder getProxyRequestEntityAccount(String pathPostfix, HttpMethod method, String accountId);

	void clearTestCache();

	boolean huisartsenInCache();

	boolean zorginstellingeninCache();

	boolean mammografenInCache();

	String cacheVullingInfo();
}
